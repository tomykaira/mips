(*NOMINCAML let xor = (lxor) in *)
let random_source =
  let rs = Array.create 4 0 in
  rs.(0) <- 123456789;
  rs.(1) <- 362436069;
  rs.(2) <- 521288629;
  rs.(3) <- 88675123;
  rs
in

let rec xor128 _ =
  let t = xor random_source.(0) ((lsl) random_source.(0) 11) in
  random_source.(0) <- random_source.(1);
  random_source.(1) <- random_source.(2);
  random_source.(2) <- random_source.(3);
  random_source.(3) <- xor (xor random_source.(3) ((lsr) random_source.(3) 19)) (xor t ((lsr) t 8));
  random_source.(3)
in

let rec random_int max=
  ((mod) ((land) (xor128 ()) (((lsl) 1 31) - 1)) max)
in

print_int ((mod) 5 2);
print_int ((mod) 5 (-2));
print_int ((mod) (-5) 2);
print_int ((mod) (-5) (-2));
print_int ((land) 9 5);
print_int (((lsl) 1 15) - 1);
print_int (random_int 10000)
