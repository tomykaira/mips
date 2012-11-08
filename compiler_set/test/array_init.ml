(*NOMINCAML let array_init = Array.init in *)
let rec f x = x + x in
let a = array_init 10 f in
print_int a.(0);
print_int a.(9)
