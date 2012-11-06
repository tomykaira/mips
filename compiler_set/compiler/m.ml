(* customized version of Map *)

module M =
  Map.Make
    (struct
      type t = Id.t
      let compare = compare
    end)
include M

let add_list xys env = List.fold_left (fun env (x, y) -> add x y env) env xys
let add_list2 xs ys env = List.fold_left2 (fun env x y -> add x y env) env xs ys
let add_list_matcher matcher typ env =
  let add_type_variables matcher typ =
    match matcher with
      | Syntax.ListWithNil(variables) -> List.map (fun v -> (v, Type.Var(typ))) variables
      | Syntax.ListWithoutNil(variables) ->
	let reversed   = List.rev variables in
	let list_var   = List.hd reversed in
	let other_vars = List.tl reversed in
	(list_var, Type.List(typ)) :: (List.map (fun v -> (v, Type.Var(typ))) other_vars)
  in
  add_list (add_type_variables matcher typ) env
