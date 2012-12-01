type t = { variable : Id.t; offset : int }

let is_spilled spills id =
  List.exists (fun {variable = v; _} -> id = v) spills

let variables = List.map (fun x -> x.variable)
