let rec f x =
  match x with
    | 1 -> print_int 2
    | 2 -> print_int 4
    | 3 -> print_int 8
    | 4 -> print_int 16
    | 5 -> print_int 32
    | 6 -> print_int 16
    | _ -> print_int 0
in
f 1;
f 2;
f 3;
f 4;
f 5;
f 0
