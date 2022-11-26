(* let generate model_proc model_file impl_file = *)
let generate _ _ _ interp_env =
  (* Definitions are separated because they can't be macro-expanded *)

  (* The structure of this test creates implicit dependencies that the 
     implementation has to fulfill, like having a setup and teardown method
     and having action methods that correspond to the model. Even the 
     requirement of being a class is an implicit dependency. This will cause
     the test to fail though. *)

  (* Todo - generate arguments for Actions *)
  (* let cert_props_defs = {|
    def toName(attr: Attribute):
      attr.name
    end

    def propGen(type: Type):
      case type:
        Primitive: 5 end
        Schema(name): Schemas.get(name).attributes.map(toName) end
      end  
    end

    def toRefinementProperty(action: Action):
      typescript:
        // Construct Model and Impl
        let model = new Model();
        let impl = new Impl();

        await impl.setup();

        // Gen args
        let newAct = prop.generate({{ propGen(action.args[0].type )}})

        // Call action
        let modelRes = model.OpenAccount(newAct);
        let implRes = impl.OpenAccount(newAct);

        await impl.teardown();
      end
    end
  |} in *)

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
      typescript:
        {{ action.args.map(toStr) }}
      end
    end
  |} in

  let cert_props = {|
    typescript:
      {{ Model.actions.map(toRefinementProperty) }}
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

