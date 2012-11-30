let ($) f g x = f (g x)

let uncurry2 f (x, y) = f x y

let concat_map f l = List.concat (List.map f l)

let zip xs ys =
  let min_length = min (List.length xs) (List.length ys) in
  List.map2 (fun x y -> (x, y)) (BatList.take min_length xs) (BatList.take min_length ys)
