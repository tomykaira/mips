(* customized version of Map *)
module Make(Ord:Map.OrderedType) = struct
  module M =
    Map.Make(Ord)
  include M

  let add_list xys env = List.fold_left (fun env (x, y) -> add x y env) env xys
  let add_pair (k, v) env = add k v env
  let add_list2 xs ys env = List.fold_left2 (fun env x y -> add x y env) env xs ys
  let from_list l = add_list l M.empty

end
