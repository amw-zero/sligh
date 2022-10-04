open Edsl

(* let nested = {|
let y = 5
let x = typescript:
  let tsvar = {{
    let again = typescript: {{y}} end
  }}
  5
end
y
|} *)

(* let plain = {|
let x = typescript:
  let t = 1
  let t2 = 2
  let t3 = {{
    typescript:
      5
    end
  }}
end
y
|} *)

let action = {|
domain Test:
  state: Int

  def change(a: Int):
    state.create!(5)
  end
end

typescript:
  let x = 5
end

change(Test, 5, 6)
|}

(* let action = {|
domain Test:
  state: Int

  def change(a: Int):
    state.create!(5)
  end
end

domain Test2:
  state: String

  def change(a: String):
    state.create!(a)
  end
end

effect create!(s: idk):
  client = typescript:
    fetch()
  end

  server = typescript:

  end
end

environment:
  def 
  client = typescript:

  end
end

Test.change(5)
|} *)

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
      #*[
        Model.schemas.map(|schema|
          typescript:
            interface #[schema.name] {
              // Note: Need splicing here
              #*[schema.attributes.map(|attr|
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
*)

(* let () = Util.evaluate_e plain *)
let () = Util.evaluate_e action
