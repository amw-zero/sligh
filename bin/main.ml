open Edsl

let ts = {|
  let y = 5
  let x = typescript: 
    let tsvar = {{y}}
  end
|}

let () = Util.evaluate_e ts
