(* let generate model_proc model_file impl_file = *)
let generate _ _ _ interp_env =
  let cert_props_defs = {|
    def toActionName(action: Action):
      action.name
    end
  |} in

  let cert_props = {|
    typescript:
      {{ Model.actions.map(toActionName) }}
    end
  |} in

  let lexbuf_defs = Lexing.from_string cert_props_defs in
  let lexbuf_props = Lexing.from_string cert_props in

  let defs_stmts = Parse.parse_with_error lexbuf_defs in
  let props_stmts = Parse.parse_with_error lexbuf_props in
  let interp_env = List.fold_left Interpreter.build_env interp_env defs_stmts in

  let ts = Interpreter.evaln props_stmts interp_env in
  match ts with
  | VTS(tss) -> File.output_tsexpr_list "refine_cert" tss
  | _ -> print_endline "Not TS"

