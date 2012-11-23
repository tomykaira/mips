let c = ref (Array.create 5 (0, 0, 0)) in
let rec iter n =
  if n < 5 then
    (!c.(n) <- (n, n, n); iter (n + 1))
  else
    ()
in
iter 0;
let (a, b, c) = !c.(4) in
print_int a;
print_int b;
print_int c
