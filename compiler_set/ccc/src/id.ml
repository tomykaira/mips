(*pp deriving *)
type t = string                         (* name *)
    deriving (Show)
type v = V of string | G of string      (* variable *)
    deriving (Show)
type l = L of string                    (* label for GOTO *)
    deriving (Show)

let counter = ref 0
let unique s =
  incr counter;
  Printf.sprintf "%s.%d" s !counter

let gen () =
  incr counter;
  V (Printf.sprintf "c.%d" !counter)

let gen_label prefix =
  incr counter;
  L (Printf.sprintf "%s.%d" prefix !counter)

let raw = function
  | V(raw_name) | G(raw_name) -> raw_name
