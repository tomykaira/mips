let rec f _ = (1, (2, 3)) in
let (a, b) = f () in
let (b, c) = b in
print_int (a + b + c)
