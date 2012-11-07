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
  add_list (Syntax.add_type_variables matcher typ) env

let show env =
  String.concat ", " (List.map (function (k, v) -> k ^ ": " ^ v)(M.bindings env))
