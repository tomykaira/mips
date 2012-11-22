(*NOMINCAML let Array.init = Array.init in *)
let rec f x = x + x in
let rec g x = if x > 0 then 0.5 +. g (x-1) else 0.0 in
let a = Array.init 10 f in
let b = Array.init 10 g in
print_int a.(0);
print_char '\n';
print_int a.(9);
print_char '\n';
if b.(9) > 3. then print_int 1 else print_int 0
