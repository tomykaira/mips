let x = [] in
let x = 1 :: 2 :: 4 :: x in
let h :: t = x in
(if t == [] then
  print_int 0
else
  print_int 1);
let y :: z :: l = t in
if l == [] then
  print_int 1
else
  print_int 0
