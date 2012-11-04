let pi = 3.141592 in
let center = 42 in
let density = 20 in

let rec rangeEach10 s f =
  f (s + 0);
  f (s + 1);
  f (s + 2);
  f (s + 3);
  f (s + 4);
  f (s + 5);
  f (s + 6);
  f (s + 7);
  f (s + 8);
  f (s + 9)
in

let rec rangeEach s e f =
  if s + 9 < e then (rangeEach10 s f; rangeEach (s + 10) e f)
  else if s < e then (f s; rangeEach (s + 1) e f)
  else ()
in

let rec color r g b =
  let array = Array.create 3 r in
  (array.(1) <- g; 
   array.(2) <- b;
   array)
in
let rec line _ =
  let array = Array.create (center * 2) (color 0 0 0) in
  let rec setColor i = array.(i) <- color 0 0 0 in
  rangeEach 0 (center * 2) setColor;
  array
in
let field =
  let array = Array.create (center * 2) (line()) in
  let rec setLine i = array.(i) <- line() in
  rangeEach 0 (center * 2) setLine;
  array
in

let rec drawPoint x deg =
  let rad = (2. *. pi *. float_of_int deg /. float_of_int density) in
  let dist = float_of_int x in
  let dx = int_of_float (dist *. cos rad +. dist *. dist *. 0.02) in
  let dy = int_of_float (dist *. sin rad) in
  field.(center + dy).(center + dx) <- color 255 (x * 10) (250-x * 10)
in

let rec drawCurve deg =
  let rec callDraw dist = drawPoint dist deg in
  rangeEach 3 25 callDraw
in

let rec print_triple triple =
  print_int triple.(0);
  print_char 32;
  print_int triple.(1);
  print_char 32;
  print_int triple.(2);
  print_char 10; 
in

let _ = (
  print_char 80;
  print_char 51;
  print_newline ();

  print_int (center * 2);
  print_char 32;
  print_int (center * 2);
  print_char 32;
  print_int 255;
  print_newline (); (* XxY*)

  rangeEach 0 density drawCurve; 

  let rec drawLine x =
    let rec printPoint y = print_triple field.(y).(x) in
    rangeEach 0 (center * 2) printPoint in
  rangeEach 0 (center * 2) drawLine
) in 0
