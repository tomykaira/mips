(*pp deriving *)
type t = string                         (* name *)
    deriving (Show)
type l = L of string                    (* label for GOTO *)
    deriving (Show)

let counter = ref 0
let genid s =
  incr counter;
  Printf.sprintf "%s.%d" s !counter
