open Edsl

(* let working = {|
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
  let x = i
end

environment:
  client:
    typescript:
      class Env {
        {{ x: Int }}
        {{ y: Int }}
      }
    end
  end

  server:
    typescript:
      app.post(5)
    end
  end
end

|} *)

(* let eval = {|
entity Todo:
  name: String
end

domain Test:
  todos: Todo

  def change(t: Todo):
    todos.create!(t)
  end
end

process client:
  typescript:
    class Client {
      todos: Todo[]
      {{ List.map(Model.variables, toTsClassProperty) }}

      {{ List.map(Model.actions, toTsAction) }}
      async change() {
        let resp = await fetch("/whatever");
        let data = await resp.json();
      }
    }
  end
end
|} *)

let eval = {|
entity Todo:
  name: String
end

domain Test:
  todos: Todo

  def change(t: Todo):
    todos.create!(t)
  end
end

def func(i: Int):
  i
end

process client:
  typescript:
    something.call({{ func(8) }})
  end
end
|}

(* 
  TODO: 
    Compile-time evaluation of Sligh terms.
      This also will require creating the special variable "Model" which is the
      compiled model.
      Replace tsclassdef_of_expr with syntax construction functions, a la template
        Haskell and Nim. Have to create TSClassProp for example, and that context
        can't be created in a quasi-quote.
*)

let () = Compiler.compile eval;
