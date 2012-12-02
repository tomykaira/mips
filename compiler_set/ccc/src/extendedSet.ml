module Make(Ord:Set.OrderedType) = struct
  module S =
    Set.Make(Ord)
  include S

  let of_list l = List.fold_left (fun s e -> add e s) empty l
  let of_option = function
    | Some(i) -> singleton i
    | None -> empty
  let unions = List.fold_left union empty
end
