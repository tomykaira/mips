type id = int
      deriving (Show)

type 'a entity =
  | E of (id * 'a)
      deriving (Show)

let id_counter = ref 0
let identify insts =
  List.map (fun i -> id_counter := !id_counter + 1; E(!id_counter, i)) insts

let unidentify (xs : 'a entity list) = List.map (fun (E(_, x)) -> x) xs
