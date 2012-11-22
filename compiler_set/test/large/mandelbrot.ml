let rec test x y =
  let abs = 2. *. 2. in
  let rec escapeq z_x z_y iter =
    if z_x *. z_x +. z_y *. z_y >= abs then true
    else if iter = 0 then false
    else escapeq (x +. z_x *. z_x -. z_y *. z_y) (y +. 2. *. z_x *. z_y) (iter-1)
  in
  escapeq 0.0 0.0 200
in

let rec mandelbrot start_x start_y step_x step_y end_x end_y =
  let rec loop_y y =
    let rec loop_x x =
      let next_x = x +. step_x in
      print_char (if test x y then 48 else 49);
      if next_x > end_x then () else loop_x next_x
    in
    let next_y = y +. step_y in
    loop_x start_x;
    print_newline ();
    if next_y > end_y then () else loop_y next_y
  in
  loop_y start_y
in

let _ = (
  print_char 80;
  print_char 49;
  print_newline ();

  print_int 301;
  print_char 32;
  print_int 201;
  print_newline (); (* XxY*)

  mandelbrot (-2.0) (-1.0) 0.01 0.01 (1.0) (1.0)
) in 0
