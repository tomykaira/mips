let x = Array.create 10 (4, 3.1, 5) in
let (p, q, r) = x.(4) in
print_int p;
print_newline ();
print_int (int_of_float q);
print_newline ();
print_int r
