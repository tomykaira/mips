let x = Array.create 2 (0, 0) in
let y = x.(0) in
let z = x.(1) in

let rec f x =
  let (p, q) = y in
  x + p + q in

let rec h x = 
  let (p, q) = z in
  x + p + q in

let x = Array.create 10 (4, 3.1, 5) in
let (p, q, r) = x.(4) in
g f;
g h;
print_int p;
print_newline ();
print_int (int_of_float q);
print_newline ();
print_int (f r)
