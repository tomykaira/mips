(*INPUT
500 400 1 0 0 1.37083 1.0375 1 0.25 0.01
*)
let conf              = Array.create 10 (0.0) in
let bound             = Array.create 4 (0.0) in
let rec read_environment pos=
(
 conf.(pos) <- read_float ();
 if pos==9 then 
 (
  bound.(0) <- (conf.(3) -. conf.(5) *. conf.(7));
  bound.(1) <- (conf.(3) +. conf.(5) *. conf.(7));
  bound.(2) <- (conf.(4) -. conf.(6) *. conf.(7));
  bound.(3) <- (conf.(4) +. conf.(6) *. conf.(7));
  let ret_width=(conf.(0)+.conf.(2)*.6.0) in
  let ret_height=(conf.(1)+.conf.(2)*.6.0) in
  int_of_float (ret_width*.ret_height)
 ) 
 else 
  read_environment (pos + 1)
)
in
print_int (read_environment 0)
