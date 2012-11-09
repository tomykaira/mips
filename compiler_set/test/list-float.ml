let x = [] in
let x = 1.0 :: 2.0 :: 4.0 :: x in
let h :: t = x in
print_int (int_of_float h);
let y :: z :: [] = t in
print_int (int_of_float y);
print_int (int_of_float z)
