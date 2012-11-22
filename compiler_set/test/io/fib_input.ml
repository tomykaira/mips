let rec fib n =
  if n <= 1 then n else
  fib (n - 1) + fib (n - 2) in
print_int 170;
print_int (fib (read_int()))
