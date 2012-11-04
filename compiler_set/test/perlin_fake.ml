(* http://freespace.virgin.net/hugo.elias/models/m_perlin.htm *)
(* どうも合っていなさそうな画像が出力される *)

let noise x y =
  let n = x + y * 57 in
  let m = (n lsl 13) lxor n in
  (* 158293 should be much bigger, but it was out of int range *)
  1.0 -. (float_of_int ((m * (m * m * 15731 + 789221) + 158293) land 0x7fffffff)) /. 1073741824.0
in

let cubic_interpolate v0 v1 v2 v3 x =
  let p = (v3 -. v2) -. (v0 -. v1) in
  let q = (v0 -. v1) -. p in
  let r = v2 -. v0 in
  let s = v1 in

  ((p *. x +. q) *. x +. r) *. x +. s
in

let smooth_noise x y =

  let corners = noise (x-1) (y-1) +. noise (x+1) (y-1) +. noise (x-1) (y+1) +. noise (x+1) (y+1) in
  let sides   = noise (x-1) y     +. noise (x+1) y     +. noise x (y-1)     +. noise x (y+1)     in
  let center  = noise x y in

  (corners /. 16.0) +. (sides /. 8.0) +. (center /. 4.0)
in

let interpolated_noise x y =
  let integer_x = int_of_float x in
  let fractional_x = x -. (floor x) in
  let integer_y = int_of_float y in
  let fractional_y = y -. (floor y) in

  let v1 = smooth_noise integer_x       integer_y       in
  let v2 = smooth_noise (integer_x + 1) integer_y       in
  let v3 = smooth_noise integer_x       (integer_y + 1) in
  let v4 = smooth_noise (integer_x + 1) (integer_y + 1) in

  cubic_interpolate v1 v2 v3 v4 (fractional_x *. fractional_y)
in

let noise_generator persistence octaves = 
  fun x y ->
    let p = persistence in
    let rec loop total i =
      if i < octaves then
        let fi         = float_of_int i in
        let frequency  = 2.0 ** fi in
        let amplitude  = p ** fi in
        let next_total = total +. interpolated_noise (x *. frequency) (y *. frequency) *. amplitude in
        loop next_total (i + 1)
      else
        total
    in
    loop 0.0 1
in

(* run *)
let noise = noise_generator 0.2 4 in
let draw _ =
  let slice = 500 in
  let inc = 0.1 in
  let max = 255.0 in
  let rec y_loop y =
    let rec x_loop x =
      print_int (int_of_float (noise x y *. max));
      print_char ' ';
      if x >= 50.0 then () else x_loop (x +. inc)
    in
    x_loop 0.0;
    print_newline (); 
    if y >= 50.0 then () else y_loop (y +. inc)
  in

  print_char 'P'; print_char '2'; print_newline ();
  print_int slice; print_char ' '; print_int slice; print_newline (); (* XxY*)
  print_int (int_of_float max); print_newline (); (* 255 *)
  
  y_loop 0.0;
  print_newline ()
in

draw ()
