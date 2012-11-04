let x = 1.23 in
let y = 4.56 in
let z = x *. y in
print_int (truncate (1000000. *. (z +. z +. z)))
