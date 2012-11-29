type i = [ `I of int ]
type f = [ `F of int ]

let int_zero = "$r0"

let show = function
  | `I(num) -> "$r" ^ string_of_int num
  | `F(num) -> "$f" ^ string_of_int num
