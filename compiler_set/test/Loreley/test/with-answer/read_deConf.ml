(*INPUT
9 0 0.4
*)
let deConf            = Array.create 3 0.0 in
let read_deConf ()=
  deConf.(0) <- read_float ();
  deConf.(1) <- read_float ();
  deConf.(2) <- read_float ()
in
read_deConf();
print_int5 deConf.(0);
print_int5 deConf.(1);
print_int5 deConf.(2)
