open Core
open Process

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

let uniq_selector_from_type env typ =
  match typ with
  | STCustom(n) ->
    let attrs = Env.SchemaEnv.find n env in
    let id_attr = List.find (fun a -> match a.typ with 
      | STGeneric(n, _) -> n = "Id"
      | _ -> false )
      attrs in

    Some(TSObject([{oname="selector"; oval=TSClosure(
      [TSPTypedAttr({tsname="e"; tstyp=TSTCustom("any")})],
      [TSReturn(
        TSIden({iname=Printf.sprintf "e.%s" id_attr.name; itype=None})
      )],
      false
    )}]))
  | _ -> None

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
    (match uniq_selector_from_type env typ with
    | Some(selector) -> TSMethodCall("fc", "uniqueArray", [typ_gen; selector])
    | None -> TSMethodCall("fc", "uniqueArray", [typ_gen]))
  | "Array" ->
    let typ = List.nth typs 0 in 
    let typ_gen = gen_type env typ in
    TSMethodCall("fc", "array", [typ_gen])
  | "Id" ->
    (* Pass through ID type - it's just there to add information to an existing type, and has
       no semantics of its own. *)
    let typ = List.nth typs 0 in 
    gen_type env typ
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
    Printf.sprintf "expect(modelResult.%s)" attr.name,
    "toEqual",
    [TSAccess(
      TSMethodCall("impl", "getState", []),
      TSIden({iname=attr.name; itype=None})
    )]
  ))

let to_db_setup attr =
  Core.({oname=attr.name; oval=to_db_access attr})

let to_client_state_setup attr =
  Core.({oname=attr.name; oval=to_state_access attr})  

let to_impl_arg attr =
  Core.({oname=attr.name; oval=TSAccess(TSIden({iname="state"; itype=None}), TSIden({iname=attr.name; itype=None}))})

let model_action_name act =
  Printf.sprintf "%sModel" act.action_ast.aname

let model_action_in_state act =
  List.concat [act.action_ast.args; act.state_vars]

  (* The property-based test body - the actual test logic lives here*)
  (* Note about cache coherence - the client state can start out incoherent, since any client can modify the server state
     But, after an action, the client must be coherent with server state. *)
let test_body act =
  let create_impl = TSLet("impl", TSFuncCall("makeStore", [])) in

  let impl_set_state_args = List.map to_impl_arg act.state_vars in
  let set_impl_state_client = TSMethodCall("impl", "setState", [TSObject(impl_set_state_args)]) in

  let set_impl_state_server = TSAwait(TSMethodCall("impl.getState()", "setDBState", [TSObject(List.map to_db_setup act.state_vars)])) in

  let model_action_props = List.map to_db_setup act.state_vars in
  let model_state_props = List.map to_client_state_setup act.action_ast.args in
  let model_action_in_props = List.concat [
    model_action_props;
    model_state_props;
  ] in
  let model_action_args = TSObject(model_action_in_props) in
  let invoke_action_model = TSLet("modelResult", TSFuncCall(model_action_name act, [model_action_args])) in
  
  let get_impl_state = TSLet("implState", TSMethodCall("impl", "getState", [])) in
  let action_args = List.map to_state_access act.action_ast.args in
  let invoke_action_impl = TSAwait(TSMethodCall("implState", act.action_ast.aname, action_args)) in
  
  (* let apply_refinement_mapping = TSNum(5) in *)
  let assert_results = List.map assert_state_var act.state_vars in

  List.concat [
    [
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

  (* The per-action test *)
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

let schema_to_interface name attrs =
  let schema_properties = List.map (fun a -> Codegen.tstyped_attr_of_typed_attr a) attrs in

  TSInterface(name, schema_properties)

  (* The local state that an action operates on. It consists of the state variables that
     the action reads and modifies, the action's arguments, as well as implementation-specific variables
     such as database state and infrastructure errors. *)
let action_type action =
  let action_type_name = action_type_name action in
  let properties = List.map (fun attr -> Codegen.tstyped_attr_of_typed_attr attr) (action_state action) in

  TSInterface(action_type_name, properties)

let db_type act = 
  act.state_vars  

let db_type_ts act = 
  let properties = List.map to_tstyped_attr (db_type act) in

  TSInterface(db_type_name act, properties)


let model_action_in_type_name act =
  Printf.sprintf "%sModelIn" act.action_ast.aname

let model_action_out_type_name act =
  Printf.sprintf "%sModelOut" act.action_ast.aname

let model_action_out_state act =
  act.state_vars

let model_action_in_type act =
  let action_type_name = model_action_in_type_name act in
  let properties = List.map (fun attr -> Codegen.tstyped_attr_of_typed_attr attr) (model_action_in_state act) in

  TSInterface(action_type_name, properties)

let model_action_out_type act =
  let action_type_name = model_action_out_type_name act in
  let properties = List.map (fun attr -> Codegen.tstyped_attr_of_typed_attr attr) (model_action_out_state act) in

  TSInterface(action_type_name, properties) 

let to_model_action act env =
  let new_args = TSPTypedAttr({tsname="params";tstyp=TSTCustom(model_action_in_type_name act)}) in
  let param_renames = List.map (fun a ->
    Core.(Let(a.name, Access(Iden("params", None), a.name)))
  ) (model_action_in_state act) in
  
  let new_body_exprs = List.concat [param_renames; act.action_ast.body] in

  let return_stmt = [TSReturn(TSObject(
    List.map
      (fun a -> {
        oname=Core.(a.name);
        oval=TSIden({iname=a.name; itype=None})
      })
      (model_action_out_state act)
  ))] in
  let new_body = List.concat [
    List.map (fun e -> Codegen.tsexpr_of_expr env e) new_body_exprs;
    return_stmt
  ] in
  
  TSLet(model_action_name act, TSClosure([new_args], new_body, false))

(* Should ultimately be parameterizable by different infra / architecture 'backends'. A backend should:
   * Create a model configured at a specific state
   * Produce an implementation configured at a specific state
   * Support invoking the action
   * Support asserting on the resulting state, with refinement mapping
*)

(* Trying out witness approach
let generate_spec _ model_proc _ cert_out env =
  (* Add DB types to env so they can be generated *)
  let db_types = List.map (fun a -> Entity(db_type_name a, (List.map convert_type a.state_vars))) model_proc.actions in
  let env = List.fold_left (fun e s -> Env.add_stmt_to_env s e) env db_types in

  let to_action_test = action_test Env.(env.schemas) in
  let schema_names = List.map fst (Env.SchemaEnv.bindings Env.(env.schemas)) in
  let env_types = List.map (fun s -> schema_to_interface s (Env.SchemaEnv.find s env.schemas)) schema_names in
  let action_types = List.map action_type model_proc.actions in

  let model_action_in_types = List.map model_action_in_type model_proc.actions in
  let model_action_out_types = List.map model_action_out_type model_proc.actions in
  let model_actions = List.map (fun a -> to_model_action a env) model_proc.actions in

  let action_tests = List.map to_action_test model_proc.actions in

  let imports = {|import { expect, test } from 'vitest';
  import { makeStore } from '../lib/state';
  import fc from 'fast-check';
  |} in

  let everything = List.concat [
    env_types; 
    action_types;
    model_action_in_types;
    model_action_out_types;
    model_actions; 
    action_tests
  ] in
  
  File.output_tsexpr_list_imports cert_out env everything imports
*)

let to_expectation act ta =
  Core.(
  TSEOSExpr(TSClosure(
    [
      TSPTypedAttr({tsname="modelResult"; tstyp=TSTCustom(model_action_out_type_name act)});
      TSPTypedAttr({tsname="implState"; tstyp=TSTCustom("ClientState")})
    ],
    [TSReturn(TSObject([
      {oname="modelExpectation"; oval=TSObject([
        {oname=ta.name; oval=TSIden({iname=Printf.sprintf "modelResult.%s" ta.name; itype=None})}
      ])};
      {oname="implExpectation"; oval=TSObject([
        {oname=ta.name; oval=TSIden({iname=Printf.sprintf "implState.%s" ta.name; itype=None})}
      ])}
    ]))],
    false
  )))

let is_read_action act =
  List.length act.assignments = 0

let to_witness_element env act =
  let name = TSString(act.action_ast.aname) in
  let type_val = TSString(if is_read_action act then "read" else "write") in
  let state_gen = state_gen_from_action env act in
  let impl_setup = TSClosure(
    [TSPTypedAttr({ tsname="state"; tstyp=TSTCustom(action_type_name act)})],
    [TSReturn(TSObject(List.map to_client_state_setup act.state_vars))],
    false
  ) in
  let db_params = List.map to_db_setup act.state_vars @ List.map to_client_state_setup act.action_ast.args in
  let client_params = List.map to_client_state_setup act.state_vars @ List.map to_client_state_setup act.action_ast.args in

  let db_setup = TSClosure(
    [TSPTypedAttr({ tsname="state"; tstyp=TSTCustom(action_type_name act)})],
    [TSReturn(TSObject(db_params))],
    false
  ) in
  let client_model_arg = if not (is_read_action act) then
    TSClosure(
      [TSPTypedAttr({ tsname="state"; tstyp=TSTCustom(action_type_name act)})],
      [TSReturn(TSObject(client_params))],
      false
    ) else 
    TSIden({iname="null"; itype=None}) in
  let action_args = List.map to_state_access act.action_ast.args in
  let run_impl = TSClosure(
    [
      TSPTypedAttr({ tsname="impl"; tstyp=TSTCustom("ClientState")});
      TSPTypedAttr({ tsname="state"; tstyp=TSTCustom(action_type_name act)})
    ],
    [TSReturn(TSMethodCall("impl", act.action_ast.aname, action_args))],
    false
  ) in
  let to_expectation_f = to_expectation act in
  let expectations = TSArray(List.map to_expectation_f act.state_vars) in

    let witness_props = [
    {oname="name"; oval=name};
    {oname="type"; oval=type_val};
    {oname="stateGen"; oval=state_gen};
    {oname="implSetup"; oval=impl_setup};
    {oname="dbSetup"; oval=db_setup};
    {oname="model"; oval=TSIden({iname=model_action_name act; itype=None})};
    {oname="modelArg"; oval=db_setup};
    {oname="clientModelArg"; oval=client_model_arg};
    {oname="runImpl"; oval=run_impl};

    (* Neeed to check DB state as well - action is of type: (ClientState, DBState) -> (ClientState, DBState)
    expectations: [
      (modelResult: CreateCounterModelOut, implState: ClientState/*, state: CreateCounterType*/) => {
        return {
          modelExpectation: { counters: modelResult.counters },
          implExpectation: { counters: implState.counters },
//          dbExpectation: { counters: state.db.counters }
        };
      },
    ],
    *)
    {oname="expectations"; oval=expectations}
  ] in

  TSEOSExpr(TSObject(witness_props))

let generate_spec _ model_proc _ cert_out env =
  let db_types = List.map (fun a -> Entity(db_type_name a, a.state_vars)) model_proc.actions in
  let env = List.fold_left (fun e s -> Env.add_stmt_to_env s e) env db_types in

  let schema_names = List.map fst (Env.SchemaEnv.bindings env.schemas) in
  let env_types = List.map (fun s -> schema_to_interface s (Env.SchemaEnv.find s env.schemas)) schema_names in
  let action_types = List.map action_type model_proc.actions in

  let model_action_in_types = List.map model_action_in_type model_proc.actions in
  let model_action_out_types = List.map model_action_out_type model_proc.actions in
  let model_actions = List.map (fun a -> to_model_action a env) model_proc.actions in

  let to_witness = to_witness_element env.schemas in
  let witness = TSArray(List.map to_witness model_proc.actions) in
  let witness_decl = [TSExport(TSLet("witness", witness))] in

  let imports = {|import { ClientState } from '../lib/state';
  import fc from 'fast-check';
  |} in

  let everything = List.concat [
    env_types; 
    action_types;
    model_action_in_types;
    model_action_out_types;
    model_actions; 
    witness_decl;
  ] in
  
  File.output_tsexpr_list_imports cert_out env everything imports
