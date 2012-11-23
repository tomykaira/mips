(* int_of_float の仕様により結果がことなる。
   LORELEY 仕様では ocaml と一致、 min-rt 仕様では一致しない *)
print_int(int_of_float(506. /. (1.37083-.(-.1.37083)) *. (1.3183-.(-1.37083))));
print_char '\n';
print_int(int_of_float(506.));
print_char '\n';
print_int(int_of_float(406. /. (1.0375 -. (-.1.0375)) *. (1.0367 -. (-1.0375))));
print_char '\n';
print_int(int_of_float(405.84346988));
print_char '\n';
print_int(int_of_float(1.8));
print_char '\n';
print_int(int_of_float(0.4 *. 10000.))
