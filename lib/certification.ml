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

let gen_type_val attr = 
  TSString(Core.(attr.name))

let action_test act = 
  let test_args = [] in
  let test_body = List.map gen_type_val act.args in

  TSMethodCall("Deno", "test", [TSClosure(test_args, test_body, true)])


let to_interface_property state_var =
  let tstyp = Codegen.tstype_of_sltype (Some(state_var.typ)) in

  Core.({ tsname=state_var.name; tstyp=Option.value tstyp ~default:(TSTCustom("no type")) } )

let action_type action =
    let action_name = action.action_ast.aname in
    let action_type_name = Printf.sprintf "%sType" action_name in
    let state_vars = List.map to_interface_property action.state_vars in
    let args = List.map to_interface_property action.action_ast.args in

    let properties = List.concat [state_vars; args] in

    TSInterface(action_type_name, properties)

let generate_spec _ model_proc _ cert_out env =
  let action_types = List.map action_type model_proc.actions in
  (* let test_ts = List.map action_test (List.map (fun a -> a.action_ast) model_proc.actions) in *)


  (* 
    For each action:
      * Create Deno.test block
      * create all argument data for action
      * create system state
      * Invoke action on model and impl
      * Cmopare results with refinement mapping
  *)
  
  File.output_tsexpr_list cert_out env action_types
