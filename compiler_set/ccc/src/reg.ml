open Util

type i = [ `I of int ]
    deriving (Show)
type f = [ `F of int ]
    deriving (Show)

module RegS = ExtendedSet.Make
  (struct
    type t = i
    let compare = compare
   end)

open RegS

let int_zero = `I 0
let frame = `I 1
let heap_pointer = `I 2

(* To calculate store address in MemoryAllocation *)
let address = `I 30

let show = function
  | `I(num) -> "$r" ^ string_of_int num
  | `F(num) -> "$f" ^ string_of_int num

let all_registers = of_list (List.map (fun num -> `I(num)) (range 3 29))
let available_registers = all_registers
let available_count = cardinal all_registers

let rest used = elements (diff all_registers (of_list used))

let ret = `I 3

let assign_params ids =
  zip ids (elements all_registers)

let nth_arg ids =
  `I (ids + 3)
