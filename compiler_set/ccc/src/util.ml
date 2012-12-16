let ($) f g x = f (g x)

let uncurry2 f (x, y) = f x y

let concat_map f l = List.concat (List.map f l)

let option_to_list = (function Some(x) -> [x] | None -> [])

let concat_option option =
  concat_map option_to_list option

let zip xs ys =
  let min_length = min (List.length xs) (List.length ys) in
  List.map2 (fun x y -> (x, y)) (BatList.take min_length xs) (BatList.take min_length ys)

let rec range i j = if i > j then [] else i :: (range (i+1) j)

let rev_assoc value xs =
  fst (List.find (fun (_, v) -> v = value) xs)
