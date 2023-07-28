open Core
open Process
(* open Process *)
(* let generate model_proc model_file impl_file = *)

(* Todo:
   
   * Generate test body for each action
   * Each test body generates all of the model and state data,
     then passes that data into the model and the impl, and checks that
     the refinement property holds

*)

let generate _ _ _ cert_out interp_env env =
  (* Definitions are separated because they can't be macro-expanded *)

  (* The structure of this test creates implicit dependencies that the 
     implementation has to fulfill, like having a setup and teardown method
     and having action methods that correspond to the model. Even the 
     requirement of being a class is an implicit dependency. This will cause
     the test to fail though. *)

  let cert_props_defs = {|
    def toName(attr: Attribute):
      attr.name
    end

    def toSchemaValueGenerator(schema: Schema):
      s.attributes.map(toName)
    end

    def toStr(attr: TypedAttribute):
      case attr.type:
        | Schema(s): toSchemaValueGenerator(s)
        | String(): "String"
        | Int(): "Int"
        | Decimal(): "Decimal"
      end
    end

    def toRefinementProperty(action: Action):
      action.args.map(toStr)
    end
  |} in

  let cert_props = {|
    typescript:
      {{* Model.actions.map(toRefinementProperty) }}
    end
  |} in

  let lexbuf_defs = Lexing.from_string cert_props_defs in
  let lexbuf_props = Lexing.from_string cert_props in

  let defs_stmts = Parse.parse_with_error lexbuf_defs in
  let props_stmts = Parse.parse_with_error lexbuf_props in
  let interp_env = List.fold_left Interpreter.build_env interp_env defs_stmts in

  let ts = Interpreter.evaln props_stmts interp_env in
  match ts with
  | VTS(tss) -> File.output_tsexpr_list cert_out env tss
  | _ -> print_endline "Not TS"

let action_type_name act =
  Printf.sprintf "%sType" act.action_ast.aname  

let gen_type t = match t with
| STInt -> TSMethodCall("fc", "integer", [])
| STString -> TSMethodCall("fc", "string", [])
| STDecimal -> TSMethodCall("fc", "decimal", [])
| STBool -> TSMethodCall("fc", "boolean", [])
(* Will need to look up schema from Env, and generate object form that *)
| STCustom(_) -> TSMethodCall("fc", "custom", [])
| STGeneric(_, _) -> TSMethodCall("fc", "generic", [])
| STVariant(_, _) -> TSMethodCall("fc", "variant", [])

let to_obj_prop_gen attr = 
  Core.({ oname=attr.name; oval=gen_type attr.typ})

let db_type_name act =
  Printf.sprintf "%sDBState" act.action_ast.aname  

(* TypedAttrs for infrastructure state, so it can be shared across a few different operations *)
let infra_state act =  
  Core.([
    {name="db"; typ=STCustom(db_type_name act)}
  ])

  (* The local state for an action *)
let action_state act =
  List.concat([act.state_vars; act.action_ast.args; infra_state act])

let state_gen_from_action act =
  let properties = List.map to_obj_prop_gen (action_state act) in

  TSMethodCall("fc", "record", [TSObject(properties)])

let action_test act = 
  let action_type = action_type_name act in
  let property_body = [TSNum(4)] in
  let property_check = TSMethodCall("fc", "asyncProperty", [
    TSIden({iname="state"; itype=None});
    TSClosure(
      [TSPTypedAttr({tsname="state"; tstyp=TSTCustom(action_type)})],
      property_body,
      true
    )
  ]) in
  let state_gen = TSLet("state", state_gen_from_action act) in
  let assertion = TSAwait(TSMethodCall("fc", "assert", [property_check])) in
  let test_name = Printf.sprintf "Test local action refinement: %s" act.action_ast.aname in
  TSFuncCall("test", [TSString(test_name); TSClosure([], [state_gen; assertion], true)])


let to_tstyped_attr attr =
  let tstyp = Codegen.tstype_of_sltype (Some(attr.typ)) in

  Core.({ tsname=attr.name; tstyp=Option.value tstyp ~default:(TSTCustom("no type")) } )

let schema_to_interface name attrs =
  let schema_properties = List.map to_tstyped_attr attrs in

  TSInterface(name, schema_properties)

  (* The local state that an action operates on. It consists of the state variables that
     the action reads and modifies, the action's arguments, as well as implementation-specific variables
     such as database state and infrastructure errors. *)
let action_type action =
  let action_type_name = action_type_name action in
  let properties = List.map to_tstyped_attr (action_state action) in

  TSInterface(action_type_name, properties)

let db_type act = 
  let properties = List.map to_tstyped_attr act.state_vars in

  TSInterface(db_type_name act, properties)

let generate_spec _ model_proc _ cert_out env =
  let schema_names = List.map fst (Env.SchemaEnv.bindings Env.(env.schemas)) in
  let env_types = List.map (fun s -> schema_to_interface s (Env.SchemaEnv.find s env.schemas)) schema_names in
  let action_types = List.map action_type model_proc.actions in
  let action_tests = List.map action_test model_proc.actions in
  let db_types = List.map db_type model_proc.actions in
  let imports = {|import { expect, test } from 'vitest';
  import { makeStore } from '../lib/state';
  import { Counter } from '../lib/model';
  import fc from 'fast-check';
  |} in

  let everything = List.concat [env_types; db_types; action_types; action_tests] in
 
  (* 
    For each action:
      * Create Deno.test block
      * create all argument data for action
      * create system state
      * Invoke action on model and impl
      * Cmopare results with refinement mapping
  *)
  
  File.output_tsexpr_list_imports cert_out env everything imports
