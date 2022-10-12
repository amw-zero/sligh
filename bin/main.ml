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

let eval = {|
def func(i: Int):
  i
end

process client:
  typescript:
    something.call({{ func(5) }})
  end
end
|}

(* let eval = {|
domain Test:
  state: Int

  def change(a: Int):
    state.create!(5)
  end
end

def func(i: Int):
  i
end

process client:
  def testing():
    5
  end

  typescript:
    {{ Model.map(5) }}
    {{ func(8) }}
  end
end
|} *)

(* 
  TODO: 
    Compile-time evaluation of Sligh terms.
      This will require interpreting Sligh terms in Ocaml.
      This may require taking a naming context as an arg in multiple places.
      This also will require creating the special variable "Model" which is the
      compiled model.
      Replace tsclassdef_of_expr with syntax construction functions, a la template
        Haskell and Nim. Have to create TSClassProp for example, and that context
        can't be created in a quasi-quote.

    For environment, should the typescript expressions be evaluated during
    parsing or after? If after, need to represent Sligh terms inside of TS,
    which isn't possible right now.

    I think that's pretty much necessary anyway, since the Environment has to use definitions
    from the fully built Model.
*)

let () = Compiler.compile eval;
