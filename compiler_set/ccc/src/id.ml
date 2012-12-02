(*pp deriving *)
type t = string                         (* name *)
    deriving (Show)
type v = V of string                    (* variable *)
         | G of string
         | A of string
    deriving (Show)
type l = L of string                    (* label for GOTO *)
    deriving (Show)

module TStruct = struct
  type t = string
  let compare = compare
end

module VStruct = struct
  type t = v
  let compare = compare
end

module LStruct = struct
  type t = l
  let compare = compare
end

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
  | V(raw_name) | G(raw_name) | A(raw_name) -> raw_name
