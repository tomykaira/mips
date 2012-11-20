let calc x y =
  let weight = 0.273309 in
  let sinx=sin x in
  let cosx=cos x in
  let sinhy=sinh y in
  let coshy=cosh y in
  let v=2.0 /. ((cos (2.0*.x)) +. (cosh (2.0*.y))) in
  (weight*.v*.cosx*.coshy,weight*.v*.sinx*.sinhy)
in
let (x, y) =  (calc 0.186776 (-1.835202)) in
print_int (int_of_float (10000.0 *. x));
print_char ' ';
print_int (int_of_float (10000.0 *. y))
