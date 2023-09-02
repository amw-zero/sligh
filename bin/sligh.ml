open Edsl

let usage_msg = {|| Certifying specifcation:
    sligh <model_spec> -w <cert_out>

| Model transformation:
    sligh <model_spec> -transform <transformation_script> -out <out_file>

| Certifying model transformation (deprecated):
    sligh <model_spec> -cert <cert_out> -impl <impl_out>    
|}
let cert_out = ref ""
let impl_out = ref ""
let impl_in = ref ""
let input_file = ref ""
let transform_script = ref ""
let out_file = ref ""

let set_input filename = input_file := filename

(* Possibly create toml config file for build config, like multiple transforms *)
let args = [
  ("-w", Arg.Set_string cert_out, "Output file for witness");
  ("-impl", Arg.Set_string impl_out, "Implementation output file name");
  ("-transform", Arg.Set_string transform_script, "The transformation script to run");
  ("-out", Arg.Set_string out_file, "The output file to write a transformation to");
  ("-include", Arg.Set_string impl_in, "Implementation include file name");
]

let main = begin
  Arg.parse args set_input usage_msg;  

  if !cert_out <> "" then
    Compiler.compile_cert_test !input_file !cert_out
  else if !transform_script <> "" then
    Compiler.compile_model_transform !input_file !transform_script !out_file
  else if !impl_out <> "" then
    Compiler.compile_cert_model_transform !input_file !impl_out
  else
    let out_file_name = if !out_file = "" then "model" else !out_file in
    Compiler.compile_model !input_file out_file_name
end

let () = main
