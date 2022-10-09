open Edsl

(* let simple = {|
entity Todo:
  name: String
  completed: Bool
end

domain TodoMVC:
  todos: State(Todo)

  def createTodo(t: CreateTodo):
    todos.create!(t)
  end

  def markCompleted(todoId: TodoId):
    todos.find(todoId).completed := true <=> todos.complete!(todoId)
  end
end

effect create!(todos: State(todo), t: CreateTodo):
  model:
    todos.push(t)
  end

  client:
    typescript:
      let resp = fetch("todos", { json, body: JSON.serialize({{ t }}) });
      let data = await resp.json();
    

      ENV.todos.push(data);
    end
  end

  server:
    typescript:
      app.post({{ }})
    end
  end
end

environment:
  def toTsProperty(v: Variable)
    typescript:
      {{ v.name }}: {{ v.type }}
    end
  end

  def toTsInterface(s: Schema)
    typescript:
      interface {{ s.name }} {
        {{ s.state.map(toTsProperty) }}
      }
    end
  end

  def toTsArg(arg: Attribute)
    typescript:
      {{ arg.name }}: {{ arg.typ }}
    end
  end

  def toTsAction(a: Action)
    typescript:
      async {{ a.name }}({{ a.args.map(toTsArg) }}) {
        {{ a.applyEffects!(:client) }}
      }
    end
  end

  def toTsActions(s: Schema)
    s.actions.map(toTsAction)    
  end

  client:
    typescript:
      {{ Model.domains.map(toTsInterface) }}

      class Client {
        constructor(config: (a: Client) => void = () => {}) {
          config(this);
        }

        {{ Model.domains.map(toTsProperty) }}

        {{ Model.domains.flatMap(toTsActions) }}
      }
    end
  end
end

(* let environment = {|
entity Todo:
  name: String
  completed: Bool
end

domain TodoMVC:
  todos: State(Todo)

  def createTodo(t: CreateTodo):
    todos.create!(t)
  end

  def markCompleted(todoId: TodoId):
    todos.complete!(todoId)
  end
end

environment:
  def toTsProperty(v: Variable)
    typescript:
      {{ v.name }}: {{ v.type }}
    end
  end

  def toTsInterface(s: Schema)
    typescript:
      interface {{ s.name }} {
        {{ s.state.map(toTsProperty) }}
      }
    end
  end

  def toTsArg(arg: Attribute)
    typescript:
      {{ arg.name }}: {{ arg.typ }}
    end
  end

  client:
    typescript:
      {{ Model.domains.map(toTsInterface) }}
    end
  end
end

(* let env_simpl = {|
entity Todo:
  name: String
  completed: Bool
end

domain TodoMVC:
  todos: Todo

  def createTodo(t: CreateTodo):
    todos.create!(t)
  end

  def markCompleted(todoId: TodoId):
    todos.complete!(todoId)
  end
end

def domainToTsState(d: Domain)
  d.name: d.type
end

environment:
  client:
    typescript:
      class Env {
        {{ Model.domains.map(domainToTsState) }}
      }
    end
  end
end
|} *)

let working = {|
domain Test:
  state: Int

  def change(a: Int):
    state.create!(5)
  end
end

environment:
  client:
    typescript:
      class Env {
        {{ x: Int }}
       }
    end
  end
end

|}

(* environment:
  client:
    typescript:
      class Env {
        {{ x: int }}
       }
    end
  end
end *)

let () = Compiler.compile working;
