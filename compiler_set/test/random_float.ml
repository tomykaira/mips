(* random_gen seems to return too large float value *)
let random_source =
  let rs = Array.create 4 0 in
  rs.(0) <- 123456789; 
  rs.(1) <- 362436069;
  rs.(2) <- 521288629; 
  rs.(3) <- 88675123;
  rs
in

let rec xor128 _ =
  let t = xor random_source.(0) ((lsl) random_source.(0) 11) in
  random_source.(0) <- random_source.(1);
  random_source.(1) <- random_source.(2);
  random_source.(2) <- random_source.(3);
  random_source.(3) <- xor (xor random_source.(3) ((lsr) random_source.(3) 19)) (xor t ((lsr) t 8));
  random_source.(3)
in

let rec random_float max=
  (float_of_int ((land) (xor128 ()) 8388607)) /. (float_of_int 8388607) *. max
in

let random_gen ()=
(random_float 2.0) -. 1.0
in

let iter n =
  if n > 0 then
    let random = random_gen () in
    (if random > 1.0 or random < -1.0 then
      (print_int (int_of_float (random *. 10000.)); print_char '\n')
    else
      ()); 
    iter (n-1)
  else ()
in
iter 10000000
