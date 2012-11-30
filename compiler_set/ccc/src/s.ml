(* customized version of Set *)

module S =
  Set.Make
    (struct
      type t = Id.v
      let compare = compare
     end)
include S

let of_list l = List.fold_left (fun s e -> add e s) empty l
let of_option = function
  | Some(i) -> singleton i
  | None -> empty
let unions = List.fold_left union empty
