(* Various pieces of information about parsed source, such as
   data and type definitions  *)

open Core

module SchemaEnv = Map.Make(String)

type env = {
  schemas: typed_attr list SchemaEnv.t
}

let empty_env = {
  schemas=SchemaEnv.empty
}

let add_stmt_to_env stmt env = match stmt with
| Entity(name, attrs) ->
  { schemas=SchemaEnv.add name attrs env.schemas }
| _ -> env
