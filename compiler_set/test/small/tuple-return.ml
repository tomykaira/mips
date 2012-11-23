(* to call function forcefully *)
let f x =
  let a = Array.create 3 0 in
  let g a =
    a.(0) <- a.(2); a.(1) <- a.(2) * 2;  a.(2) <- a.(2) * 3;
    (a.(0), a.(1) * 2, a.(2) * 3) in
  a.(0) <- x; a.(1) <- x; a.(2) <- x; g a
in
let (a, b, c) = f 5 in
print_int a;
print_int b;
print_int c
