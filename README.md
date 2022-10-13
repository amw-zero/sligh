# Model-driven Implementation Derivation and Test Generation

This is a pivot on the idea in [Sligh](https://github.com/amw-zero/sligh). The same general goal applies: the language should enable a [model-driven workflow](https://concerningquality.com/model-based-testing/) that automates as much of the implementation and test generation and possible.

The main difference of this approach is that here, implementation derivation happens via metaprogramming vs. being compiled without any intervention. This places more burden of work on the programmer, but there's basically infinite control on the generated output.

As an example (this won't fully compile today, but is the overall goal):

**Model**:

```
entity Todo:
  name: String
end

domain Test:
  todos: Todo[]

  def addTodo(t: Todo.Create):
    todos.create!(t)
  end
end
```

This is a simple model of a todo app. `create!` now denotes an _effect_ which is an operation that has different definitions in the model and implementation. Effects are hand-implemented via metaprogramming:

**Effects**:
```
// Effect definition - how is an effect executed
// across implementation components?
effect create!<'a>(state: SystemState<'a>, val: 'a):
  model: 
    state.push(val)
  end

  // Client code generated from model definitions
  client:
    typescript:
      let resp = fetch({{ state.name }}, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({{ val }}),
      });
      let data = await resp.json();

      client.todos.push(data);
    end
  end

  server:
    // Generate endpoint code in same way
  end
end
```

Here we generated the client implementation of a `create!` effect, which simply sends a POST request to the proper endpoint. The key here is that we're defining the client definition in a `typescript` block, which is a quasi-quotation block for defining TypeScript code. `{{ ... }}` unquotes / splices, so the TS client can be generated from definitions in the model.

And putting it all together, we need to define the top-level application processes that are the entry point for each system component:

**Processes**:
```
// Helpers to convert model definitions to TS
def toTsTypedIden(var: Variable)
  typescript:
    {{ var.name }}: {{ var.type }}
  end
end

def toTsAction(act: Action)
  typescript:
    async {{ act.name }}({{ act.args.map(toTsTypedIden) }}) {
      {{ act.body }}
    }
  end
end

// Generates a client.ts file.
//
// This is a stateful class which holds all of the state variables
// declared the model, and since toTsAction is called in a client block 
// here, the client effect definition is used as the action body.
process client:
  typescript:
    class Client {
      {{ Model.variables.map(toTsTypedIden) }}

      {{ Model.actions.map(toTsAction) }}
    }
  end
end

// Generates a server.ts file
process server:
  // generate web server bootstrap
end
```

I omitted the server code for brevity, but that can all be generated from the Model in the same way.

We didn't cover test generation, but with even less help, a model-based test can be generated here since we have all of the definitions that we need to compare actions in the implementation and model.