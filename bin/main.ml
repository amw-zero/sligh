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

  def change(a: int):
    state.push(5)
  end
end

y
|}

(* let () = Util.evaluate_e plain *)
let () = Util.evaluate_e action
