open Util

type i = [ `I of int ]
type f = [ `F of int ]

module RegSet =
  Set.Make
    (struct
      type t = i
      let compare = compare
     end)
open RegSet

let of_list l = List.fold_left (fun s e -> add e s) empty l
let of_option = function
  | Some(i) -> singleton i
  | None -> empty
let unions = List.fold_left union empty

let int_zero = `I 0
let frame = `I 1
let heap_pointer = `I 2

let show = function
  | `I(num) -> "$r" ^ string_of_int num
  | `F(num) -> "$f" ^ string_of_int num

let rec range i j = if i > j then [] else i :: (range (i+1) j)
let all_registers = of_list (List.map (fun num -> `I(num)) (range 3 29))

let rest used = elements (RegSet.diff all_registers (of_list used))

let ret = `I 3

let assign_params ids =
  zip (elements all_registers) ids
