(* customized version of Set *)

module S =
  Set.Make
    (struct
      type t = Id.t
      let compare = compare
     end)
include S

let of_list l = List.fold_left (fun s e -> add e s) empty l
let add_list l s = List.fold_left (fun s e -> add e s) s l
let add_list_matcher matcher env =
  add_list (Syntax.matcher_variables matcher) env
