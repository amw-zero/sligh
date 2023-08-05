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

let rec gen_type env t = match t with
| STInt -> TSMethodCall("fc", "integer", [])
| STString -> TSMethodCall("fc", "string", [])
| STDecimal -> TSMethodCall("fc", "decimal", [])
| STBool -> TSMethodCall("fc", "boolean", [])
| STCustom(n) -> 
  let attrs = Env.SchemaEnv.find n env in
  let properties = List.map (fun a -> to_obj_prop_gen env a) attrs in
  TSMethodCall("fc", "record", [TSObject(properties)])
| STGeneric(n, typs) -> gen_generic n typs env
| STVariant(_, _) -> TSMethodCall("fc", "variant", [])

and to_obj_prop_gen env attr = 
  Core.({ oname=attr.name; oval=gen_type env attr.typ})

and gen_generic name typs env =
  match name with
  | "Set"  -> 
    let typ = List.nth typs 0 in 
    let typ_gen = gen_type env typ in
    TSMethodCall("fc", "array", [typ_gen])
  | "Array" ->
    let typ = List.nth typs 0 in 
    let typ_gen = gen_type env typ in
    TSMethodCall("fc", "array", [typ_gen])
  | _ -> TSMethodCall("fc", "generic", [])  

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

let state_gen_from_action env act =
  let act_to_obj_prop = to_obj_prop_gen env in
  let properties = List.map act_to_obj_prop (action_state act) in

  TSMethodCall("fc", "record", [TSObject(properties)])

let to_tstyped_attr attr =
  let tstyp = Codegen.tstype_of_sltype (Some(attr.typ)) in

  Core.({ tsname=attr.name; tstyp=Option.value tstyp ~default:(TSTCustom("no type")) } )  

  (* Accesses a part of the state from the db *)
let to_db_access attr =
  TSAccess(TSIden({iname="state.db"; itype=None}), TSIden({iname=Core.(attr.name); itype=None}))

  (* Accesses a part of the client-side *)
let to_state_access attr =
  TSAccess(TSIden({iname="state"; itype=None}), TSIden({iname=Core.(attr.name); itype=None}))

let assert_state_var attr =
  Core.(
  TSMethodCall(
    Printf.sprintf "expect(model.%s)" attr.name,
    "toEqual",
    [TSAccess(
      TSMethodCall("impl", "getState", []),
      TSIden({iname=attr.name; itype=None})
    )]
  ))

let to_db_setup attr =
  Core.({oname=attr.name; oval=to_db_access attr})

let to_impl_arg attr =
  (* object propr *)
  Core.({oname=attr.name; oval=TSAccess(TSIden({iname="state"; itype=None}), TSIden({iname=attr.name; itype=None}))})

  (* The property-based test body - the actual test logic lives here*)
  (* Note about cache coherence - the client state can start out incoherent, since any client can modify the server state
     But, after an action, the client must be coherent with server state. *)
let test_body act =
  let model_state_args = List.map to_db_access act.state_vars in
  let create_model = TSLet("model", TSNew("CounterApp", model_state_args)) in

  let create_impl = TSLet("impl", TSFuncCall("makeStore", [])) in

  let impl_set_state_args = List.map to_impl_arg act.state_vars in
  let set_impl_state_client = TSMethodCall("impl", "setState", [TSObject(impl_set_state_args)]) in

  let set_impl_state_server = TSMethodCall("impl", "setDBState", [TSObject(List.map to_db_setup act.state_vars)]) in

  let action_args = List.map to_state_access act.action_ast.args in
  let invoke_action_model = TSMethodCall("model", act.action_ast.aname, action_args) in
  
  let get_impl_state = TSLet("implState", TSMethodCall("impl", "getState", [])) in
  let invoke_action_impl = TSAwait(TSMethodCall("implState", act.action_ast.aname, action_args)) in
  
  (* let apply_refinement_mapping = TSNum(5) in *)
  let assert_results = List.map assert_state_var act.state_vars in

  List.concat [
    [create_model;
    create_impl;
    set_impl_state_client;
    set_impl_state_server;
    invoke_action_model;
    get_impl_state;
    invoke_action_impl;
    ];
    assert_results
    (* apply_refinement_mapping; *)
  ]

  (* The actual per-action test *)
let action_test env act = 
  let action_type = action_type_name act in
  let property_body = test_body act in
  let property_check = TSMethodCall("fc", "asyncProperty", [
    TSIden({iname="state"; itype=None});
    TSClosure(
      [TSPTypedAttr({tsname="state"; tstyp=TSTCustom(action_type)})],
      property_body,
      true
    )
  ]) in
  let state_gen = TSLet("state", state_gen_from_action env act) in
  let assertion = TSAwait(TSMethodCall("fc", "assert", [property_check])) in
  let test_name = Printf.sprintf "Test local action refinement: %s" act.action_ast.aname in
  TSFuncCall("test", [TSString(test_name); TSClosure([], [state_gen; assertion], true)])


(* This probably shouldn't exist. But Sligh Sets get compiled to TS Arrays in Codegen, and
    that has to happen here too or the test doesn't typecheck. *)
    let convert_type attr = 
    match attr.typ with
    | STGeneric(n, typs) -> if n = "Set" then {name=attr.name; typ=STGeneric("Array", typs)} else attr
    | _ -> attr  

let schema_to_interface name attrs =
  let schema_properties = List.map (fun a -> to_tstyped_attr (convert_type a)) attrs in

  TSInterface(name, schema_properties)

  (* The local state that an action operates on. It consists of the state variables that
     the action reads and modifies, the action's arguments, as well as implementation-specific variables
     such as database state and infrastructure errors. *)
let action_type action =
  let action_type_name = action_type_name action in
  let properties = List.map (fun attr -> to_tstyped_attr (convert_type attr)) (action_state action) in

  TSInterface(action_type_name, properties)

let db_type act = 
  act.state_vars  

let db_type_ts act = 
  let properties = List.map to_tstyped_attr (db_type act) in

  TSInterface(db_type_name act, properties)

(* Should ultimately be parameterizable by different infra / architecture 'backends'. A backend should:
   * Create a model configured at a specific state
   * Produce an implementation configured at a specific state
   * Support invoking the action
   * Support asserting on the resulting state, with refinement mapping
*)
let generate_spec _ model_proc _ cert_out env =
  (* Add DB types to env so they can be generated *)
  let db_types = List.map (fun a -> Entity(db_type_name a, (List.map convert_type a.state_vars))) model_proc.actions in
  let env = List.fold_left (fun e s -> Env.add_stmt_to_env s e) env db_types in

  let to_action_test = action_test Env.(env.schemas) in
  let schema_names = List.map fst (Env.SchemaEnv.bindings Env.(env.schemas)) in
  let env_types = List.map (fun s -> schema_to_interface s (Env.SchemaEnv.find s env.schemas)) schema_names in
  let action_types = List.map action_type model_proc.actions in
  let action_tests = List.map to_action_test model_proc.actions in

  let imports = {|import { expect, test } from 'vitest';
  import { makeStore } from '../lib/state';
  import { CounterApp } from './model';
  import fc from 'fast-check';
  |} in

  let everything = List.concat [
    env_types; 
    action_types; 
    action_tests
  ] in
  
  File.output_tsexpr_list_imports cert_out env everything imports
