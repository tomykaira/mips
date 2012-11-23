let rec inprod v1 v2 i =
  if i < 0 then 0.0 else
	  (
	  print_int i;
	  print_newline ();
	  print_int (truncate (10000.0 *. v1.(i)));
	  print_newline ();
	  print_int (truncate (10000.0 *. v2.(i)));
	  print_newline ();
	  print_int (truncate (10000.0 *. v1.(i) *. v2.(i)));
	  print_newline ();
	  v1.(i) *. v2.(i) +. inprod v1 v2 (i - 1) ) in
let v1 = Array.create 3 1.23 in
let v2 = Array.create 3 4.56 in
print_int (truncate (1000000. *. inprod v1 v2 2))
