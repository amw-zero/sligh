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

let eval = {|
entity Todo:
  note: String
end



def toName(a: Schema):
  a.name
end

def toTsClassBody(a: Attribute):
  tsClassProp(a.name, a.type)
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
|}

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
      { name: "todo", type: Custom(Todo) }
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
*)

let () = Compiler.compile eval;
