(*pp deriving *)
type t = string                         (* name *)
    deriving (Show)
type l = L of string                    (* label for GOTO *)
    deriving (Show)

let counter = ref 0
let genid s =
  incr counter;
  Printf.sprintf "%s.%d" s !counter

let gen () =
  incr counter;
  Printf.sprintf "c.%d" !counter

let gen_label prefix =
  incr counter;
  L (Printf.sprintf "%s.%d" prefix !counter)
