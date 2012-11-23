let test x =
  print_int (int_of_float(x *. 1000.));  print_char ' ';
  print_int (int_of_float((exp x) *. 1000.)); print_char '\n' in
let rec iter start =
  if start < 0.6931 then
    (test start; iter (start +. 0.01))
  else
    ()
in
iter (0.)
  
