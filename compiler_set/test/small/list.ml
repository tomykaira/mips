let x = [] in
let x = 1 :: 2 :: 4 :: x in
let h :: t = x in
print_int h;
let y :: z :: [] = t in
print_int y;
print_int z
