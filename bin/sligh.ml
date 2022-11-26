open Edsl

(* let working = {|
entity Other:
  val: Int
end

domain Test:
  state: Int

  def change(a: Int):
    state.create!(5)
  end
end

def topLevel():
  let x = 5
end

def other(i: Int):
  i
end

process client:
  typescript:
    class Env {
      {{ x: Int }}
      {{ y: Int }}
    }
  end
end

process server:
  typescript:
    app.post({{ other(5) }})
  end
end

|} *)

(* 
   
let model = new Model();
let client = new Client();

schema Action:
  name: String
  args: Array(TypedAttr)
  body: SlighExpr
end
*)

let processes = {|
entity Account:
  balance: Int
  name: String
end

entity Transaction:
  srcAccount: Account
  dstAccount: Account
  amount: Decimal
end

process Accounts:
  accounts: Account

  def OpenAccount(newAct: Account):
    5
  end

  def UpdateBalance(act: Account, balance: Decimal):
    6
  end
end

process Ledger:
  transactions: Transaction

  def Transfer(srcAct: Account, dstAct: Account, amount: Decimal):
    7
  end
end

def toImplMethod(action: Action):
  tsClassMethod(action.name, action.args, action.body)
end

def toImplAttr(attr: TypedAttribute):
  tsClassProp(attr.name, attr.type)
end

def toCtorArg(attr: TypedAttribute):
  tsTypedAttr(attr.name, attr.type)
end

def toCtorBodyStmt(attr: TypedAttribute):
  let this = tsIden("this")
  let attrIden = tsIden(attr.name)
  let target = tsAccess(this, attrIden)

  tsAssignment(target, attrIden)
end

def impl():
  let methods = Model.actions.map(toImplMethod)
  let attrs = Model.variables.map(toImplAttr)
  
  let ctorArgs = Model.variables.map(toCtorArg)
  let ctorBody = Model.variables.map(toCtorBodyStmt)
  let ctor = tsClassMethod("constructor", ctorArgs, ctorBody)

  let defs = attrs.concat(methods)
  let nextDefs = append(ctor, defs)

  tsClass("Client", nextDefs)
end

implementation:
  typescript:
    {{ impl() }}
  end
end

file another:
  typescript: let x = {{ "test" }} end
end

|}


(* 
let eval = {|
entity Todo:
  note: String
end

def toName(a: Schema):
  a.name
end

def toTsClassBody(a: Attribute):
  Action(tsClassProp(a.name, a.type))
end

def toTsClass(s: Schema):
  let body = s.attributes.map(toTsClassBody)
  
  typescript:
    {{ tsClass(s.name, body) }} 
  end
end

def attrName(a: Attribute):
  a.name
end

process client:
  typescript:
    let allModelNames = {{ Model.schemas.map(toName) }}
    let allTypes = {{ Model.schemas.map(toTsClass )}}
  end
end

process server:
  typescript:
    let todoName = {{ Todo.name }}
    let todoAttrs = {{ Todo.attributes.map(attrName) }}
  end
end

process test:
  typescript:
    
  end
end
|} *)

(*

{
  Todo: {
    name: Todo,
    attrs: [
      { name: "name", type: Primitive(String) },
    ]
  },
  Todos: {
    name: Todos,
    attrs: [
      { name: "todo", type: Custom(Todo) }j
    ]
  },
  Model: {
    name: Model,
    attrs: [
      { name: "schemas", type: Array(Schema([Todo, Todos])) }
    ]
  }
}   


*)

(* next process client:
  typescript:
    let names = {{ Model.schemas.map(toName) }}
  end

  def toTsClass(a: Schema):
    tsclass(a.name, )

    typescript:
      tsclass 
      class {{ a.name }} {

      }
    end
end
end *)

(* 
  TODO: 
    * Effect system. Algebraic effects?
      - Convenient syntax for inlining a handler in the model, so model functionality
        is apparent without looking anywhere else, i.e. handle todos.create!(t) with todos.push(t)
      - Handle effects at thte process level, i.e. process client: handle create! with clientCreate
        this is how effects are "overridden" per each process
    * Model conformance test
      - This requires marking Actions in the implementation. Otherwise, how to create test?
*)

let () = Compiler.compile processes;

(* let _ = {|
  entity Data:
    val: Int
  end

  process Idk:
    def func(arg: Data):
      5
    end
  end

  def toTypeName(attr: TypedAttribute):
    case attr.type:
      | Schema(schema): schema.name
      | Int(): "test"
    end
  end

  def toOutput(action: Action):
    action.args.map(toTypeName)
  end

  implementation:
    typescript:
      let x = {{ Idk.actions.map(toOutput) }}
    end
  end
|} *)

(* let () = Compiler.compile(str_test); *)

(* let () = print_endline (Interpreter.string_of_value (Compiler.interp str_test)); *)

(* let result = Compiler.interp str_test
let str = match result with
| VTS(tss) -> String.concat "\n\n" (List.map Codegen.string_of_ts_expr tss)
| _ -> "unknown" *)

(* let () = print_endline str; *)
