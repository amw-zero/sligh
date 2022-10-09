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

(* let impl = {|
domain Test:
  state: Int

  def change(a: Int):
    state.create!(5)
  end
end

effect create!(state: 'a, n: Int):
  client:
    typescript:
      let resp = await fetch(#[relationFrom(state)], {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(n),
      });
      let data = await resp.json();

      // ??
      // #[action.transferedStateVariabels.map(|sv|:
      //   typescript: client.#[sv.name]
      // end)]
    end
  end

  server:
    typescript:
      app.post(...)
    end
  end
end

implementation:
  client:
    typescript:
      // Make all schemas available on the Client
      #[
        Model.schemas.map(|schema|
          typescript:
            interface #[schema.name] {
              // Note: Need splicing here
              #[schema.attributes.map(|attr|
                typescript: #[attr.name]: #[attr.type] end
              )]
            }
          end
        )
      ]

      class Client {
        constructor(config: (a: Client) => void = () => {}) {
          config(this);
        }

        #*[
          Model.transferredStateVariables.map(|sv|
            typescript: 
              #[sv.name]: #[sv.type]
            end
          )
        ]

        #*[ Model.actions.map(|action| 
          typescript:
            async #[action.name](#*[separatedList(',' action.args)]) {
              // Since the effect definition separates client and server
              // functionality, the client side can be chosen here
              // The applyEffects function replaces all effects in a term 
              // with the specified argument's definition, i.e. 'applyEffects(client)'
              // replaces all effect invocations with the client definition.
              await #[ action.body.applyEffects(client) ];
            }
          end
        )]
      }
    end
  end

  server:
    applyEffects(server)
  end
end
|} *)

(*
  Todo:
    * Closures
    * Unquote splicing (#*[] vs #[])
    * applyEffects
    * separatedList
    * Model - Model.actions, Model.transferredStateVariables
    * 
*)

(*
  Closure syntax study:
   let c = |x| x + 1
   let c2 = (x): x + 1
   let c3 = (x: Int) -> Int: x + 1 end
   let mapped = [1,2,4].map (x): x + 1 end
*)

(*
  Unquote syntax study:
    let c = typescript: let x = sligh: 5 end end
    let indented = typescript: 
      let x = sligh: 
        5
      end 
    end

    let unquoted = typescript: let x = unquote: 5 end end
    let unquote_nested = typescript:
      let x = unquote:
        5
      end
    end
*)
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

environment:
  client:
    typescript:
      let x = {{ Model.domains }}
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

domain Test2:
  state: Int

  def change(a: Int):
    state.create!(5)
  end
end

typescript:
  let x = 5
end

environment:
  client:
    typescript:
      let x = {{Model.domains()}}
    end
  end
end

|}

let () = Compiler.compile working;
