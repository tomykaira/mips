(* customized version of Map *)

module M =
  Map.Make
    (struct
      type t = Id.v
      let compare = compare
     end)
include M

let add_list xys env = List.fold_left (fun env (x, y) -> add x y env) env xys
let add_pair (k, v) env = add k v env
let add_list2 xs ys env = List.fold_left2 (fun env x y -> add x y env) env xs ys
let from_list l = add_list l M.empty

let show env =
  String.concat ", " (List.map (function (Id.V k, v) -> k ^ ": " ^ v
    | (Id.G k, v) -> "G" ^ k ^ ": " ^ v )(M.bindings env))
