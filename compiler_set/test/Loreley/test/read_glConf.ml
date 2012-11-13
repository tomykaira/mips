(*INPUT
788800 0
*)
let glConf            = Array.create 2 0 in

let read_glConf ()=
  (glConf.(0)<-read_int ();glConf.(1)<-read_int ())
in

read_glConf ();
print_int glConf.(0); print_char ' ';  print_int glConf.(1)
