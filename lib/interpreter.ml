(* 
  An interpreter for Sligh programs. This is initially intended for compile-time 
  execution in unquoting within quasi-quoted expressions, but can also be used as
  a general interpreter.
*)

open Core

module Env = Map.Make(String)

let model_var_name = "Model"

type primitive_typ =
  | PTNum
  | PTString
  | PTDecimal

type value =
  | VNum of int
  | VString of string
  | VBool of bool
  | VNil
  | VArray of value list
  | VFunc of func_def
  | VMacro of proc_effect list
  | VType of type_val
  | VInstance of instance
  | VVoid

  (* Syntax values *)
  | VSLExpr of expr
  | VTS of tsexpr list
  | VTSExpr of tsexpr
  | VTSClassDef of tsclassdef
  | VTSTypedAttr of tstyped_attr
  (* Object patterns - for function args *)
  | VTSObjectPat of tsobject_pat
  | VTSObjectPatProp of tsobject_pat_prop

  (* Object values *)
  | VTSObjectProp of obj_prop
  | VTSType of ts_type
  | VTSSymbolImport of tssymbol_import

and schema = {
  sname: string;
  attrs: schema_attr list;
}

and schema_attr = {
  aname: string;
  typ: type_val;
}

and instance_attr = {
  iname: string;
  ivalue: value;
}

and instance = instance_attr list

and variant_tag_v = {
  vtname: string;
  vtattrs: schema_attr list
}

and variant_v = {
  vvname: string;
  vcases: variant_tag_v list;
}

and type_val = 
| VSchema of schema
| VTVariant of variant_v
| VTArray of type_val list
| VPrimitive of primitive_typ
| VTGeneric of string * type_val list

let rec string_of_value v = match v with
  | VNum(n) -> string_of_int n
  | VBool(b) -> if b then "true" else "false"
  | VNil -> "nil"
  | VFunc(fd) -> Printf.sprintf "<func %s>" fd.fdname
  | VType(tv) -> string_of_type_val tv
  | VArray(vs) -> Printf.sprintf "[%s]" (String.concat ", " (List.map string_of_value vs))
  | VTS(_) -> "VTS"
  | VTSExpr(tse) -> Util.string_of_ts_expr tse
  | VTSTypedAttr(ta) -> Util.string_of_ts_typed_attr ta
  | VTSClassDef(_) -> "VTSClassDef"
  | VString(s) -> s
  | VInstance(attrs) -> Printf.sprintf "{%s}" (String.concat ", " (List.map string_of_instance_attr attrs))
  | VSLExpr(e) -> Util.string_of_expr e
  | VMacro(_) -> "Macro"
  | VVoid -> "Void"
  | VTSObjectProp({oname; oval}) -> Printf.sprintf "%s: %s" oname (Util.string_of_ts_expr oval)
  | VTSType(t) -> Util.string_of_tstype(t)
  | VTSObjectPat(_) -> "ObjectPat"
  | VTSObjectPatProp(_) -> "ObjectPatProp"
  | VTSSymbolImport(_) -> "symbol import"
and string_of_instance_attr attr = Printf.sprintf "%s: %s" attr.iname (string_of_value attr.ivalue)
and string_of_type_val tv = match tv with
  | VSchema s -> string_of_schema s
  | VTArray(_) -> "VTArray"
  | VPrimitive(_) -> "PrimitiveType"
  | VTVariant(_) -> "VTVariant"
  | VTGeneric(_) -> "VTGeneric"
and string_of_schema_attr a =
  Printf.sprintf "%s: %s" a.aname (string_of_type_val a.typ)
and string_of_schema s =
  Printf.sprintf "schema %s\n\  %s\nend" s.sname (Util.print_list "\n" (List.map (fun a -> string_of_schema_attr a) s.attrs))

let val_as_str v = match v with
| VString(s) -> s
| _ -> failwith (Printf.sprintf "Expected string: %s" (string_of_value v))

let val_as_int v = match v with
| VNum(i) -> i
| _ -> failwith (Printf.sprintf "Expected string: %s" (string_of_value v))

let val_as_type_val v = match v with
| VType(tv)-> tv
| _ -> failwith (Printf.sprintf "Expected type val: %s" (string_of_value v))

let val_as_val_list v = match v with
| VArray(vs) -> vs
| _ -> failwith (Printf.sprintf "Expected array val: %s" (string_of_value v))

let val_as_tsexpr v = match v with
| VTSExpr(tse) -> tse
| _ -> failwith (Printf.sprintf "Expected TSExpr val: %s" (string_of_value v))

let val_as_tstyped_attr v = match v with 
| VTSTypedAttr(ta) -> ta
| _ -> failwith (Printf.sprintf "Expected VTSTypedAttr val: %s" (string_of_value v))

let val_as_tstyped_attr_list v = match v with
| VArray(vs) -> List.map val_as_tstyped_attr vs
| _ -> failwith (Printf.sprintf "Expected Array of TSTypedAttrs val: %s" (string_of_value v))

let val_as_tssymbol_import v = match v with
| VTSSymbolImport(si) -> si
| _ -> failwith (Printf.sprintf "Expected TSSymbolImport val: %s" (string_of_value v))

let val_as_tssymbol_import_list v = match v with
| VArray(vs) -> List.map val_as_tssymbol_import vs
| _ -> failwith (Printf.sprintf "Expected Array of TSSymbolImports val: %s" (string_of_value v))

let val_as_tsexprs v = match v with
| VTS(tses) -> tses
| _ -> failwith (Printf.sprintf "Expected TS val: %s" (string_of_value v))

let val_as_tsexpr_list v = match v with
| VArray(vs) -> List.map val_as_tsexpr vs
| _ -> failwith (Printf.sprintf "Expected Array of TSExpr vals: %s" (string_of_value v))

let val_as_slexpr v = match v with
| VSLExpr(e) -> e
| _ -> failwith (Printf.sprintf "Expected SLExpr val: %s" (string_of_value v))

let val_as_tsobj_prop v = match v with
| VTSObjectProp(op) -> op
| _ -> failwith (Printf.sprintf "Expected TSObjectProp val: %s" (string_of_value v))

let val_as_tsobj_props v = match v with
| VArray(vs) -> List.map val_as_tsobj_prop vs
| _ -> failwith (Printf.sprintf "Expected Array of TSObjectProp vals: %s" (string_of_value v))

let val_as_bool v = match v with
| VBool(b) -> b
| _ -> failwith (Printf.sprintf "Expected Bool: %s" (string_of_value v))

let val_as_object_pat_prop_list v =
  val_as_val_list v |>
  List.map (fun v ->   match v with
  | VTSObjectPatProp(opp) -> opp
  | _ -> failwith (Printf.sprintf "Expected VTSObjectPatProp: %s" (string_of_value v))) 

let val_as_tstype v = match v with
| VTSType(tst)-> tst
| _ -> failwith (Printf.sprintf "Expected TSType: %s" (string_of_value v))

type interp_env = value Env.t

type builtin = {
  bname: string;
  bdef: func_def;
}

let builtin_map_name = "map"
let builtin_map_def = {
  fdname=builtin_map_name;
  fdargs=[{name="list";typ=STString}; {name="func";typ=(STCustom("func"))}];
  fdbody=[];
}

let builtin_concat_name = "concat"
let builtin_concat_def = {
  fdname=builtin_concat_name;
  fdargs=[{name="l1";typ=STString}; {name="l2";typ=STString};];
  fdbody=[];
}

let builtin_append_name = "append"
let builtin_append_def = {
  fdname=builtin_append_name;
  fdargs=[{name="lst";typ=STString}; {name="element";typ=STString};];
  fdbody=[];
}

let builtin_appendstr_name = "appendStr"
let builtin_appendstr_def = {
  fdname=builtin_appendstr_name;
  fdargs=[{name="str";typ=STString}; {name="newStr";typ=STString};];
  fdbody=[];
}

let builtin_flatten_name = "flatten"
let builtin_flatten_def = {
  fdname=builtin_append_name;
  fdargs=[{name="lsts";typ=STString};];
  fdbody=[];
}

let builtin_index_name = "index"
let builtin_index_def = {
  fdname=builtin_index_name;
  fdargs=[{name="arr";typ=STString}; {name="i";typ=STString};];
  fdbody=[];
}

let builtin_tsclass_name = "tsClass"
let builtin_tsclass_def = {
  fdname=builtin_tsclass_name;
  fdargs=[{name="name";typ=STString}; {name="defs";typ=(STCustom("syntax"))}];
  fdbody=[];
}

let builtin_tsclassprop_name = "tsClassProp"
let builtin_tsclassprop_def = {
  fdname=builtin_tsclassprop_name;
  fdargs=[{name="name";typ=STString}; {name="typ";typ=STString}];
  fdbody=[];
}

let builtin_tsclassmethod_name = "tsClassMethod"
let builtin_tsclassmethod_def = {
  fdname=builtin_tsclassmethod_name;
  fdargs=[{name="name";typ=STString}; {name="args";typ=STString}; {name="body";typ=STString}; {name="is_async";typ=STString}];
  fdbody=[];
}

let builtin_tstyped_attr_name = "tsTypedAttr"
let builtin_tstyped_attr_def = {
  fdname=builtin_tstyped_attr_name;
  fdargs=[{name="name";typ=STString}; {name="type";typ=STString}];
  fdbody=[];
}

let builtin_tsaccess_name = "tsAccess"
let builtin_tsaccess_def = {
  fdname=builtin_tsaccess_name;
  fdargs=[{name="left";typ=STString}; {name="right";typ=STString}];
  fdbody=[];
}

let builtin_tsiden_name = "tsIden"
let builtin_tsiden_def = {
  fdname=builtin_tsiden_name;
  fdargs=[{name="str";typ=STString};];
  fdbody=[];
}

let builtin_tsassignment_name = "tsAssignment"
let builtin_tsassignment_def = {
  fdname=builtin_tsassignment_name;
  fdargs=[{name="left";typ=STString}; {name="right";typ=STString}];
  fdbody=[];
}

let builtin_tsstatement_list_name = "tsStatementList"
let builtin_tsstatement_list_def = {
  fdname=builtin_tsstatement_list_name;
  fdargs=[{name="stmts";typ=STString}];
  fdbody=[];
}

let builtin_tsinterface_name = "tsInterface"
let builtin_tsinterface_def = {
  fdname=builtin_tsinterface_name;
  fdargs=[{name="name";typ=STString}; {name="attrs";typ=STString}];
  fdbody=[];
}

let builtin_tsmethod_call_name = "tsMethodCall"
let builtin_tsmethod_call_def = {
  fdname=builtin_tsmethod_call_name;
  fdargs=[{name="receiver";typ=STString}; {name="callName";typ=STString}; {name="args";typ=STString}];
  fdbody=[];
}

let builtin_tsclosure_name = "tsClosure"
let builtin_tsclosure_def = {
  fdname=builtin_tsclosure_name;
  fdargs=[{name="args";typ=STString}; {name="body";typ=STString}; {name="is_async";typ=STString}];
  fdbody=[];
}

let builtin_tslet_name = "tsLet"
let builtin_tslet_def = {
  fdname=builtin_tslet_name;
  fdargs=[{name="left";typ=STString}; {name="right";typ=STString}];
  fdbody=[];
}

let builtin_tsobject_name = "tsObject"
let builtin_tsobject_def = {
  fdname=builtin_tsobject_name;
  fdargs=[{name="props";typ=STString};];
  fdbody=[];
}

let builtin_tsobjectprop_name = "tsObjectProp"
let builtin_tsobjectprop_def = {
  fdname=builtin_tsobjectprop_name;
  fdargs=[{name="name";typ=STString}; {name="value";typ=STString};];
  fdbody=[];
}

let builtin_tstype_name = "tsType"
let builtin_tstype_def = {
  fdname=builtin_tstype_name;
  fdargs=[{name="name";typ=STString};];
  fdbody=[];
}

let builtin_tsawait_name = "tsAwait"
let builtin_tsawait_def = {
  fdname=builtin_tsawait_name;
  fdargs=[{name="block";typ=STString};];
  fdbody=[];
}

let builtin_tsnew_name = "tsNew"
let builtin_tsnew_def = {
  fdname=builtin_tsnew_name;
  fdargs=[{name="class";typ=STString}; {name="args";typ=STString};];
  fdbody=[];
}

let builtin_tsexport_name = "tsExport"
let builtin_tsexport_def = {
  fdname=builtin_tsexport_name;
  fdargs=[{name="expr";typ=STString}];
  fdbody=[];
}

let builtin_tsimport_name = "tsAliasImport"
let builtin_tsimport_def = {
  fdname=builtin_tsimport_name;
  fdargs=[{name="imports";typ=STString}; {name="file";typ=STString}];
  fdbody=[];
}

let builtin_tssymbol_import_name = "tsSymbolImport"
let builtin_tssymbol_import_def = {
  fdname=builtin_tssymbol_import_name;
  fdargs=[{name="symbol";typ=STString}; {name="alias";typ=STString}];
  fdbody=[];
}

let builtin_tsstring_name = "tsString"
let builtin_tsstring_def = {
  fdname=builtin_tsstring_name;
  fdargs=[{name="str";typ=STString}];
  fdbody=[];
}

let all_builtins = [
  {bname=builtin_map_name; bdef=builtin_map_def};
  {bname=builtin_concat_name; bdef=builtin_concat_def};
  {bname=builtin_append_name; bdef=builtin_append_def};
  {bname="delete"; bdef={
    fdname="delete";
    fdargs=[{name="array";typ=STString}; {name="elem";typ=STString}];
    fdbody=[];
  }};
  {bname=builtin_appendstr_name; bdef=builtin_appendstr_def};
  {bname=builtin_flatten_name; bdef=builtin_flatten_def};
  {bname=builtin_index_name; bdef=builtin_index_def};
  {bname="length"; bdef={
    fdname="length";
    fdargs=[{name="array";typ=STString}];
    fdbody=[];
  }};
  {bname="greaterThan"; bdef={
    fdname="greaterThan";
    fdargs=[{name="num";typ=STString}; {name="gt";typ=STString}];
    fdbody=[];
  }};
  (* TS Syntax Methods *)
  {bname=builtin_tsclassprop_name; bdef=builtin_tsclassprop_def};
  {bname=builtin_tsclass_name; bdef=builtin_tsclassprop_def};
  {bname=builtin_tsclassmethod_name; bdef=builtin_tsclassmethod_def};
  {bname=builtin_tstyped_attr_name; bdef=builtin_tstyped_attr_def};
  {bname=builtin_tsaccess_name; bdef=builtin_tsaccess_def};
  {bname=builtin_tsiden_name; bdef=builtin_tsiden_def};
  {bname=builtin_tsassignment_name; bdef=builtin_tsassignment_def};
  {bname=builtin_tsstatement_list_name; bdef=builtin_tsstatement_list_def};
  {bname=builtin_tsinterface_name; bdef=builtin_tsinterface_def};
  {bname=builtin_tsmethod_call_name; bdef=builtin_tsmethod_call_def};
  {bname=builtin_tsclosure_name; bdef=builtin_tsclosure_def};
  {bname=builtin_tslet_name; bdef=builtin_tslet_def};
  {bname=builtin_tsobject_name; bdef=builtin_tsobject_def};
  {bname=builtin_tsobjectprop_name; bdef=builtin_tsobjectprop_def};
  {bname=builtin_tstype_name; bdef=builtin_tstype_def};
  {bname=builtin_tsawait_name; bdef=builtin_tsawait_def};
  {bname=builtin_tsnew_name; bdef=builtin_tsnew_def};
  {bname=builtin_tsexport_name; bdef=builtin_tsexport_def};
  {bname=builtin_tsimport_name; bdef=builtin_tsimport_def};
  {bname="tsDefaultImport"; bdef={
    fdname="tsDefaultImport";
    fdargs=[{name="import";typ=STString}; {name="file";typ=STString}];
    fdbody=[];
  }};
  {bname="tsObjectPat"; bdef={
    fdname="tsObjectPat";
    fdargs=[{name="props";typ=STString}; {name="type";typ=STString}];
    fdbody=[];
  }};
  {bname="tsObjectPatProp"; bdef={
    fdname="tsObjectPatProp";
    fdargs=[{name="name";typ=STString}; {name="value";typ=STString}];
    fdbody=[];
  }};
  {bname=builtin_tssymbol_import_name; bdef=builtin_tssymbol_import_def};
  {bname=builtin_tsstring_name; bdef=builtin_tsstring_def};
]

let new_environment_with_builtins () =
  let env = Env.empty in
  List.fold_left (fun env b -> Env.add b.bname (VFunc(b.bdef)) env) env all_builtins

let builtin_funcs = List.map (fun b -> b.bname) all_builtins

let val_as_instance v = match v with  
  | VInstance(inst) -> inst
  | _ -> failwith (Printf.sprintf "Expected instance, but %s is not one" (string_of_value v))

let find_attr attr_name instance = match List.find_opt (fun attr -> attr.iname = attr_name) instance with
  | Some(attr) -> Some(attr.ivalue)
  | None -> None  

let rec type_val_of_sligh_type st env =
  match st with
  | STCustom(name) -> 
    let inst = Env.find name env |> val_as_instance in
    find_attr "type" inst |> Option.get |> val_as_type_val
  | STVariant(name, _) ->
    let inst = Env.find name env |> val_as_instance in
    find_attr "type" inst |> Option.get |> val_as_type_val
  | STInt -> VPrimitive(PTNum)
  | STString -> VPrimitive(PTString)
  | STDecimal -> VPrimitive(PTDecimal)
  | STGeneric(n, types) -> VTGeneric(n, List.map (fun t -> type_val_of_sligh_type t env) types)

let typed_attr_instance attr env = VInstance([
  { iname="name"; ivalue=VString(attr.name) };
  { iname="type"; ivalue=VType(type_val_of_sligh_type attr.typ env) }
])

let action_instance (action: Process.action) env =
  VInstance([
    { iname="name"; ivalue=VString(Core.(action.action_ast.aname)) };
    { iname="args"; ivalue=VArray(List.map (fun arg -> typed_attr_instance arg env) action.action_ast.args)};
    { iname="stateVars"; ivalue=VArray(List.map (fun sv -> typed_attr_instance sv env) action.state_vars) };
    { iname="body"; ivalue=VSLExpr(StmtList(action.action_ast.body))};
  ])

let add_process_to_env name (attrs: typed_attr list) (actions: Process.action list) (env: interp_env) =
  let schema_attrs = List.map (fun attr -> typed_attr_instance attr env) attrs in
  let schema_types = List.map (fun attr -> {aname=attr.name; typ=type_val_of_sligh_type attr.typ env}) attrs in
  let action_attrs = List.map (fun action -> action_instance action env) actions in

  let instance_attrs = [
    {iname="name"; ivalue=VString(name)};
    {iname="attributes"; ivalue=VArray(schema_attrs)};
    {iname="actions"; ivalue=VArray(action_attrs)};
    {iname="type"; ivalue=VType(VSchema({ sname=name; attrs=schema_types}))}
  ] in

  Env.add name (VInstance(instance_attrs)) env

let add_schema_to_env name (attrs: typed_attr list) (env: interp_env) =
  let schema_attrs = List.map (fun attr -> typed_attr_instance attr env) attrs in
  let schema_types = List.map (fun attr -> {aname=attr.name; typ=type_val_of_sligh_type attr.typ env}) attrs in

  let instance_attrs = [
    {iname="name"; ivalue=VString(name)};
    {iname="attributes"; ivalue=VArray(schema_attrs)};
    {iname="type"; ivalue=VType(VSchema({ sname=name; attrs=schema_types}))}
  ] in

  Env.add name (VInstance(instance_attrs)) env

let variant_case_instance vt env =
  VInstance([
    {iname="name"; ivalue=VString(Core.(vt.tname))};
    {iname="attrs"; ivalue=VArray(List.map (fun ta -> typed_attr_instance ta env) Core.(vt.tattrs))};
  ])

let add_variant_to_env name (variants: variant_tag list) (env: interp_env) =
  let variant_cases = List.map (fun v -> variant_case_instance v env) variants in
  let instance_attrs = [
    {iname="name"; ivalue=VString(name)};
    {iname="cases"; ivalue=VArray(variant_cases)};
    {iname="type"; ivalue=VType(VTVariant({vvname=name; vcases=List.map
      (fun v -> {
        vtname=Core.(v.tname);
        vtattrs=List.map(fun ta -> {aname=ta.name; typ=type_val_of_sligh_type ta.typ env}) Core.(v.tattrs)
      })
      variants}))
    }
  ] in

  Env.add name (VInstance(instance_attrs)) env

let build_env env stmt = 
  match stmt with
  | FuncDef(fd) -> Env.add fd.fdname (VFunc(fd)) env
  | Process(d, defs) -> 
      let attrs: typed_attr list = Process.filter_attrs defs in
      let actions = Process.filter_actions defs in
      let analyzed_actions =  Process.analyze_actions actions attrs in
      add_process_to_env d attrs analyzed_actions env
  | Entity(e, attrs) -> 
      add_schema_to_env e attrs env
  | Effect(efct) -> Env.add efct.ename (VMacro efct.procs) env
  | _ -> env

let add_model_to_env m env =
  let schema_instances = List.map (fun schema -> Env.find Process.(schema.name) env) Process.(m.schemas) in
  let variant_instances = List.map (fun v -> Env.find Process.(v.vname) env) Process.(m.variants) in
  let variable_instances = List.map (fun variable -> typed_attr_instance variable env) Process.(m.variables) in
  let action_instances = List.map (fun action -> action_instance action env) Process.(m.actions) in

  Env.add model_var_name (VInstance([
    { iname="schemas"; ivalue=VArray(schema_instances) };
    { iname="variants"; ivalue=VArray(variant_instances) };
    { iname="variables"; ivalue=VArray(variable_instances) };
    { iname="actions"; ivalue=VArray(action_instances) }
  ])) env

let print_env env =
  print_endline "\nInterpreter.Env";
  Env.iter (fun k value -> Printf.printf "%s -> %s\n" k (string_of_value value)) env;
  print_endline ""

let build_call_env (env: interp_env) (pair: (typed_attr * value)) =
  let (arg_sig, arg) = pair in
  Env.add arg_sig.name arg env

let build_macro_env (env: interp_env) (pair: (typed_attr * expr)) =
  let (arg_sig, arg) = pair in
  Env.add arg_sig.name (VSLExpr(arg)) env

let ts_type_of_type_val tv = match tv with
  | VSchema(s) -> TSTCustom(s.sname)
  | VPrimitive(pt) -> (match pt with
    | PTNum -> TSTNumber
    | PTString -> TSTString
    | PTDecimal -> TSTNumber)
  | _ -> failwith (Printf.sprintf "Unable to convert type val to TS Type: %s" (string_of_type_val tv))

let check_branch_match v pattern = match pattern with
  | StringPattern(s) -> (match v with
    | VString(vs) -> vs = s
    | _ -> failwith (Printf.sprintf "Attempting to match a non-string value with a string pattern: %s" (string_of_value v)))
  | VariantPattern(vp) -> (match v with
    | VType(tv) -> (match tv with
      | VSchema(_) -> "Schema" = vp.vname
      | VPrimitive(pt) -> (match pt with
        | PTNum -> "Int" = vp.vname
        | PTString -> "String" = vp.vname
        | PTDecimal -> "Decimal" = vp.vname)
      | VTVariant(_) -> "Variant" = vp.vname
      | VTGeneric(_, _) -> "Generic" = vp.vname
      | _ -> failwith (Printf.sprintf "Not supporting pattern match for type: %s" (string_of_type_val tv)))
    | _ -> failwith (Printf.sprintf "Not supporting pattern match for value: %s" (string_of_value v)))

let bind_pattern_values v pattern env = match pattern with
  | StringPattern(_) -> env
  | VariantPattern(vp) -> (match v with
    | VType(tv) -> (match tv with
      | VSchema(s) ->
        let var_name = match (List.hd vp.var_bindings) with
          | PBVar(n) -> Some(n)
          | PBAny -> None in

        (match var_name with
        | Some(n) -> Env.add n (Env.find s.sname env) env
        | None -> env)
      | VTVariant({vvname; _}) ->
        let variant = Env.find vvname env |> val_as_instance in

        let env_with_name = match (List.hd vp.var_bindings) with
          | PBVar(var_name) -> Env.add var_name (VString(vvname)) env
          | PBAny -> env in

        let env_with_cases = (match (List.nth vp.var_bindings 1) with
          | PBVar(cas_name) ->
            Env.add cas_name ((find_attr "cases" variant) |> Option.get) env_with_name
          | PBAny -> env) in

        env_with_cases
      | VTGeneric(name, types) ->
        let env_with_name = match (List.hd vp.var_bindings) with
          | PBVar(var_name) -> Env.add var_name (VString(name)) env
          | PBAny -> env in

        let env_with_types = (match (List.nth vp.var_bindings 1) with
        | PBVar(cas_name) ->
          Env.add cas_name (VArray(List.map (fun t -> VType(t)) types)) env_with_name
        | PBAny -> env) in

        env_with_types

      | VPrimitive(pt) -> (match pt with
        | PTNum -> env
        | PTString -> env
        | PTDecimal -> env
      )
      | VTArray(_) -> env)
    | _ -> failwith (Printf.sprintf "Not supporting pattern match for value: %s" (string_of_value v)))

  (* Evaluate a Sligh expression *)
let rec evaln es env = eval_and_return es env
and eval (e: expr) (env: interp_env): (value * interp_env) =
  match e with
  | Iden(i, _) -> (Env.find i env, env)
  | Num(n) -> (VNum(n), env)
  | Bool(b) -> (VBool(b), env)
  | If(e, e_then, e_else) -> (match eval e env with
    | (VBool(b), env') -> if b then
        eval e_then env'
      else
        Option.value (Option.map (fun els -> eval els env') e_else) ~default: (VNil, env')
    | _ -> failwith (Printf.sprintf "Expected bool expr: %s" (Util.string_of_expr e)))
  | Call(name, args) ->
    let (arg_sigs, body) = try (match Env.find name env with
      | VFunc({fdargs;fdbody;_}) -> (fdargs, fdbody)
      | _ -> failwith "Attempted to call non function definition")
      with Not_found -> failwith (Printf.sprintf "Couldn't find function def to call: %s" name) in
    if List.length args != List.length arg_sigs then failwith (Printf.sprintf "Bad arity at func call: %s" name) else ();
    
    let reduced_args = List.map
      (fun a ->  eval a env |> fst)
      args in
    if List.exists (fun n -> n = name) builtin_funcs then
      eval_builtin_func name reduced_args env
    else
      let arg_pairs: (typed_attr * value) list = List.combine arg_sigs reduced_args in
      let call_env = List.fold_left build_call_env env arg_pairs in

      (eval_and_return body call_env, env)
  | Access(e, var) ->
      let (left, _) = eval e env in

      (match left with
      | VInstance(inst) -> 
        (match find_attr var inst with
        | Some(a) -> (a, env)
        | None -> print_endline "Didn't find"; failwith (Printf.sprintf "Could not find variable named: %s" var))
      | _ -> failwith (Printf.sprintf "Unable to access variable - did not evaluate to an instance: %s" (Util.string_of_expr e)))
  | Let(var, e) ->
    let (ve, next_env) = eval e env in
    let new_env = Env.add var ve next_env in

    (ve, new_env)
  | String(s) -> (VString(s), env)
  | Case(e, branches) ->
      let (ve, env') = eval e env in

      let result = List.find (fun branch -> check_branch_match ve branch.pattern) branches in

      eval result.value (bind_pattern_values ve result.pattern env')
  | Array(es) -> (VArray(List.map (fun e -> eval e env |> fst) es), env)
  | TS(tses) ->
    let evaled_tses = List.concat_map (fun tse -> eval_ts tse env |> fst) tses in
    (VTS(evaled_tses), env)
  | FuncDef(fd) ->
    (VVoid, Env.add fd.fdname (VFunc(fd)) env)
  | Process(d, defs) ->
      let attrs: typed_attr list = Process.filter_attrs defs in
      let actions = Process.filter_actions defs in
      let analyzed_actions =  Process.analyze_actions actions attrs in

      (VVoid, add_process_to_env d attrs analyzed_actions env)
  | Entity(e, attrs) ->
      (VVoid, add_schema_to_env e attrs env)
  | Variant(n, vs) ->
      (VVoid, add_variant_to_env n vs env)
  | Effect(efct) ->
      (VVoid, Env.add efct.ename (VMacro efct.procs) env)
  | _ -> failwith (Printf.sprintf "Eval: Unable to eval expr %s" (Util.string_of_expr e))

and eval_and_return expr_list env =
  List.fold_left (fun (_, curr_env) e -> eval e curr_env) (eval (List.hd expr_list) env) expr_list
    |> fst

and eval_builtin_func name args env =
  match name with
  | "map" -> 
    let lst_arg = List.nth args 0 in
    let map_func_arg = List.nth args 1 in 

    let fd = match map_func_arg with
      | VFunc(fd) -> fd
      | _ -> failwith (Printf.sprintf "Tried calling map builtin with non-function 2nd arg %s" (string_of_value map_func_arg)) in
    
    let lst = match lst_arg with 
      | VArray(vs) -> vs
      | _ -> failwith (Printf.sprintf "Tried calling map builtin with non-array 1st arg %s" (string_of_value lst_arg)) in
    
    let result = List.map (fun e ->
      let arg_pairs: (typed_attr * value) list = List.combine fd.fdargs [e] in
      let call_env = List.fold_left build_call_env env arg_pairs in

      eval_and_return fd.fdbody call_env
    ) lst in

    (VArray(result), env)
  | "concat" ->
    let lst1_arg = List.nth args 0 in
    let lst2_arg = List.nth args 1 in 

    let lst1 = match lst1_arg with 
      | VArray(vs) -> vs
      | _ -> failwith (Printf.sprintf "Tried calling concat builtin with non-array 1st arg %s" (string_of_value lst1_arg)) in

    let lst2 = match lst2_arg with 
    | VArray(vs) -> vs
    | _ -> failwith (Printf.sprintf "Tried calling map builtin with non-array 2nd arg %s" (string_of_value lst2_arg)) in
    
    let result = List.concat([lst1; lst2]) in

    (VArray(result), env)
  | "append" ->
    let lst_arg = List.nth args 0 |> val_as_val_list in
    let elem_arg = List.nth args 1 in

    let result: value list = List.cons elem_arg lst_arg in

    (VArray(result), env)
  | "appendStr" ->
    let str = List.nth args 0 |> val_as_str in
    let newStr = List.nth args 1 |> val_as_str in

    let result: string = str ^ newStr in

    (VString(result), env)
  | "flatten" ->
    let lists = List.nth args 0 |> val_as_val_list |> List.map val_as_val_list in

    let result: value list = List.concat lists in

    (VArray(result), env)
  | "index" ->
    let arr_arg = List.nth args 0 |> val_as_val_list in
    let idx_arg = List.nth args 1 |> val_as_int in

    (List.nth arr_arg idx_arg, env)
  | "length" ->
    let arr_arg = List.nth args 0 |> val_as_val_list in

    (VNum(List.length arr_arg), env)
  | "greaterThan" ->
    let larg = List.nth args 0 |> val_as_int in
    let rarg = List.nth args 1 |> val_as_int in
  
    (VBool(larg > rarg), env)
  | "tsClass" ->
    let name_arg = List.nth args 0 in
    let name_arg = match name_arg with
      | VString(s) -> s
      | _ -> failwith (Printf.sprintf "Calling tsClass on non-string %s" (string_of_value name_arg)) in

    let defs_arg = List.nth args 1 in
    let defs_arg = match defs_arg with
      | VArray(vcds) -> List.filter_map (fun vcd -> match vcd with | VTSClassDef(cd) -> Some(cd) | _ -> None ) vcds
      | _ -> failwith (Printf.sprintf "Calling tsClass on non-Array of VTS Class defs %s" (string_of_value defs_arg)) in

    (VTS([tsClass name_arg defs_arg]), env)
  | "tsClassProp" ->
    let name_arg = List.nth args 0 in
    let name_arg = match name_arg with
    | VString(s) -> s
    | _ -> failwith (Printf.sprintf "Calling tsClassProp on non-string %s" (string_of_value name_arg)) in
    let typ_arg = List.nth args 1 in
    let typ_arg = match typ_arg with
      | VType(tv) -> tstype_of_type_val tv
      | VTSType(tst) -> tst
      | _ -> failwith (Printf.sprintf "Calling tsClassProp on non-Type value %s" (string_of_value typ_arg)) in

    (VTSClassDef(tsClassProp name_arg typ_arg), env)
  | "tsClassMethod" ->
    let name_arg = List.nth args 0 |> val_as_str in
    let args_arg = List.nth args 1 |> val_as_val_list in
    let args_arg = List.map (fun arg -> match arg with
      | VInstance(inst) ->
        let name = match find_attr "name" inst |> Option.get with 
          | VString(n) -> n
          | _ -> failwith "should be a string" in
        let typ = match find_attr "type" inst |> Option.get with
          | VType(tv) -> ts_type_of_type_val tv
          | _ -> failwith "not a type val" in

        {tsname=name; tstyp= typ}
      | VTSTypedAttr(ta) -> ta
      | _ -> failwith (Printf.sprintf "Calling tsClassMethod, args not array of instances %s" (string_of_value arg))) args_arg in

    let body_arg = List.nth args 2 |> val_as_tsexpr_list in
    let is_async = List.nth args 3 |> val_as_bool in

    (VTSClassDef(TSClassMethod(name_arg, args_arg, body_arg, is_async)), env)
  | "tsTypedAttr" -> 
    let name_arg = List.nth args 0 |> val_as_str in
    let type_arg = List.nth args 1 in
    let ts_type = match type_arg with
    | VType(tv) -> tstype_of_type_val tv
    | VTSType(t) -> t
    | _ -> failwith (Printf.sprintf "Expected Sligh or TS Type value, but got: %s" (string_of_value type_arg)) in

    (VTSTypedAttr(tsTypedAttr name_arg ts_type), env)
  | "tsAccess" -> 
    let left_arg = List.nth args 0 |> val_as_tsexpr in
    let right_arg = List.nth args 1 |> val_as_tsexpr in

    (VTSExpr(tsAccess left_arg right_arg), env)
  | "tsLet" ->
    let name = List.nth args 0 |> val_as_str in
    let value = List.nth args 1 |> tsexpr_of_val in

    (VTSExpr(TSLet(name, value)), env)
  | "tsIden" -> 
    let name_arg = List.nth args 0 |> val_as_str in

    (VTSExpr(tsIden name_arg), env)
  | "tsType" ->
    let typ = List.nth args 0 |> val_as_str in
    let typ_val = TSTCustom(typ) in

    (VTSType(typ_val), env)
  | "tsAssignment" -> 
    let left_arg = List.nth args 0 |> val_as_tsexpr in
    let right_arg = List.nth args 1 |> val_as_tsexpr in

    (VTSExpr(tsAssignment left_arg right_arg), env)
  | "tsStatementList" -> 
    let stmts = List.nth args 0 |> val_as_tsexpr_list in

    (VTSExpr(tsStatementList stmts), env)
  | "tsInterface" -> 
    let name = List.nth args 0 |> val_as_str in
    let attrs = List.nth args 1 |> val_as_tstyped_attr_list in

    (VTSExpr(tsInterface name attrs), env)    
  | "tsMethodCall" ->
    let receiver = List.nth args 0 |> val_as_str in
    let call_name = List.nth args 1 |> val_as_str in
    let args = List.nth args 2 |> val_as_val_list |> List.map (fun v -> tsexpr_of_val v) in

    (VTSExpr(TSMethodCall(receiver, call_name, args)), env)
  | "tsClosure" ->
    let closure_args = List.nth args 0 |> val_as_val_list
      |> List.map (fun v -> match v with
        | VTSTypedAttr(ta) -> TSPTypedAttr(ta)
        | VTSObjectPat(op) -> TSPObjectPat(op)
        | _ -> failwith "Expected valid patterns in param list") in

    let body = List.nth args 1 |> val_as_val_list |> List.map tsexpr_of_val in
    let is_async = List.nth args 2 |> val_as_bool in

    (VTSExpr(TSClosure(closure_args, body, is_async)), env)
  | "tsObject" ->
    let props = List.nth args 0 |> val_as_tsobj_props in

    (VTSExpr(TSObject(props)), env)
  | "tsObjectProp" ->
    let name = List.nth args 0 |> val_as_str in
    let value = List.nth args 1 |> tsexpr_of_val in

    (VTSObjectProp({oname=name; oval=value}), env)
  | "tsObjectPatProp" ->
    let name = List.nth args 0 |> val_as_str in
    let value = List.nth args 0 |> val_as_str in

    (VTSObjectPatProp({oppname=name; oppvalue=value}), env)
  | "tsObjectPat" ->
    let props = List.nth args 0 |> val_as_object_pat_prop_list in
    let typ = List.nth args 1 |> val_as_tstype in

    (VTSObjectPat({opprops=props; optyp=typ}), env)
  | "tsAwait" ->
    let blk = List.nth args 0 |> val_as_tsexpr in

    (VTSExpr(TSAwait(blk)), env)
  | "tsNew" ->
    let cls = List.nth args 0 |> val_as_str in
    let args = List.nth args 1 |> val_as_val_list |> List.map (fun v -> tsexpr_of_val v) in

    (VTSExpr(TSNew(cls, args)), env)
  | "tsExport" ->
    let expr = List.nth args 0 |> val_as_tsexpr in

    (VTSExpr(TSExport(expr)), env)
  | "tsAliasImport" ->
    let imports = List.nth args 0 |> val_as_tssymbol_import_list in
    let file = List.nth args 1 |> val_as_str in

    (VTSExpr(TSAliasImport(imports, file)), env)

  | "tsDefaultImport" ->
    let import = List.nth args 0 |> val_as_str in
    let file = List.nth args 1 |> val_as_str in

    (VTSExpr(TSDefaultImport(import, file)), env)
  | "tsSymbolImport" ->
    let symbol = List.nth args 0 |> val_as_str in
    let alias = List.nth args 1 |> val_as_str in

    (VTSSymbolImport({symbol; alias=Some(alias)}), env)
  | "tsString" ->
    let str = List.nth args 0 |> val_as_str in

    (VTSExpr(TSString(str)), env)
  | _ -> failwith (Printf.sprintf "Attempted to call unimplemented builtin func: %s" name)

and eval_ts ts_expr env = match ts_expr with
| SLExpr(e) -> 
  let res: value = eval e env |> fst in
  ([tsexpr_of_val res], env)
| SLSpliceExpr(e) -> 
  let res: value = eval e env |> fst in
  (tsexpr_of_val_splice res, env)
| TSLet(var, exp) -> ([TSLet(var, eval_ts exp env |> fst |> List.hd)], env)
| TSMethodCall(recv, call, args) ->
    let reduced_args = List.concat_map (fun a -> eval_ts a env |> fst) args in
    ([TSMethodCall(recv, call, reduced_args)], env)
| TSFuncCall(f, args) ->
    let reduced_args = List.concat_map (fun a -> eval_ts a env |> fst) args in
    ([TSFuncCall(f, reduced_args)], env)
| TSStmtList(tses) -> ([TSStmtList(List.concat_map (fun tse -> eval_ts tse env |> fst) tses)], env)
| TSObject props -> 
  let reduced_props = List.map (fun prop -> {oname=prop.oname; oval=eval_ts prop.oval env |> fst |> List.hd}) props in
  ([TSObject(reduced_props)], env)
| TSIf(e1, e2, e3) -> (match e3 with
  | Some(else_e) -> 
    let e1' = eval_ts e1 env |> fst |> List.hd in
    let e2' = eval_ts e2 env |> fst |> List.hd in
    let e3' = eval_ts else_e env |> fst |> List.hd in

    ([TSIf(e1', e2', Some(e3'))], env)
  | None ->
    let e1' = eval_ts e1 env |> fst |> List.hd in
    let e2' = eval_ts e2 env |> fst |> List.hd in

    ([TSIf(e1', e2', None)], env))

(* These evalulate to themselves because they have no recursive nodes, i.e. are terminal *)
| TSIden _ -> ([ts_expr], env)
| TSNum _ ->  ([ts_expr], env)
| TSBool(_) -> ([ts_expr], env)
| TSClass (_, _) -> ([ts_expr], env) 
| TSArray _ -> ([ts_expr], env) 
| TSString _ -> ([ts_expr], env)
| TSAccess (_, _) -> ([ts_expr], env)
| TSAssignment (_, _) -> ([ts_expr], env)
| TSInterface (_, _) -> ([ts_expr], env)
| TSClosure (_, _, _) -> ([ts_expr], env)
| TSAwait _ -> ([ts_expr], env)
| TSExport _ -> ([ts_expr], env)
| TSAliasImport(_, _) -> ([ts_expr], env)
| TSDefaultImport(_, _) -> ([ts_expr], env)
| TSNew(_, _) -> ([ts_expr], env)

and tsexpr_of_val (v: value) = match v with
| VNum(n) -> TSNum(n)
| VTS(tss) -> TSStmtList(tss)
| VArray(vs) -> TSArray(List.map tsexpr_of_val vs)
| VString(s) -> TSString(s)
| VTSExpr(e) -> e
| VSLExpr(e) -> SLExpr(e)
| _ -> failwith (Printf.sprintf "Cannot tseval value %s" (string_of_value v))

and tsexpr_of_val_splice v = match v with
| VArray(vs) -> List.map tsexpr_of_val vs
| _ -> failwith (Printf.sprintf "Can only splice array values: %s" (string_of_value v))

and tstype_of_type_val tv = match tv with
| VSchema(s) -> TSTCustom(s.sname)
| VTArray(_) -> TSTCustom("RandomArray")
| VPrimitive(_)  -> TSTNumber
| VTVariant({vvname; _}) -> TSTCustom(vvname)
| VTGeneric(name, types) -> TSTGeneric(name, List.map tstype_of_type_val types)

let type_of_string s = match s with
  | "Int" -> STInt
  | "String" -> STString
  | "Decimal" -> STDecimal
  | _ -> STCustom(s)

let tstype_of_string s = match s with
  | "number" -> TSTNumber
  | _ -> TSTCustom(s)
