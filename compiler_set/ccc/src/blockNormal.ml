(* Normalize block *)

let normalize_variable =

let rec normalize_statement statement =
  match statement with
    | Syntax.Block(variables, statements) ->
      Block(map normalize_variable variables, concat . map normalize_statement statements)
    | Syntax.

let normalize_top t =
  match t with
    | Syntax.Function (id, return_type, params, statement) ->
      Function(id, return_type, params, normalize_statement statement)
    | Syntax.GlobalVariable (var) ->
      GlobalVariable(var)

let normalize ts =
  map normalize_top ts
