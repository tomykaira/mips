(* 浮動小数基本演算1 *)
let rec fequal a b = a = b in
let rec fless a b = (a < b) in

let rec fispos a = (a > 0.0) in
let rec fisneg a = (a < 0.0) in
let rec fiszero a = (a = 0.0) in


(* 浮動小数基本演算2 *)
let rec fabs a =
  if a < 0.0 then -. a
  else a
in
let rec abs_float x = fabs x in
let rec fneg a = -. a in
let rec fhalf a = a *. 0.5 in
let rec fsqr a = a *. a in

(* floor, int_of_float, float_of_int はlib_asm.sで定義 *)

(* 算術関数 *)
let pi = 3.14159265358979323846264 in
let pi2 = pi *. 2.0 in
let pih = pi *. 0.5 in

(* atan *)
let rec atan_sub i xx y =
  if i < 0.5 then y
  else atan_sub (i -. 1.0) xx ((i *. i *. xx) /. (i +. i +. 1.0 +. y))
in
let rec atan x =
  let sgn =
    if x > 1.0 then 1
    else if x < -1.0 then -1
    else 0
  in
  let x =
    if sgn <> 0 then 1.0 /. x
    else x
  in
  let a = atan_sub 11.0 (x *. x) 0.0 in
  let b = x /. (1.0 +. a) in
  if sgn > 0 then pi /. 2.0 -. b
  else if sgn < 0 then -. pi /. 2.0 -. b
  else b
  in

let rec atan2 y x =
  let ans = atan (y /. x) in
  if x >= 0. then
    ans
  else
    if y >= 0. then pi +. ans else ans -. pi
in

(* tan *)
let rec tan_sub i xx y =
  if i < 2.5 then y
  else tan_sub (i -. 2.) xx (xx /. (i -. y))
in
let rec tan x = (* -pi/4 <= x <= pi/4 *)
  x /. (1. -. (tan_sub 9. (x *. x) 0.0))
in

(* sin *)
let rec sin_sub1 x p =
  if x < p then p else
  let p2 = p *. 2.0 in
  (* if not p2 +. p2 > P2, p2 is nan/inf? if not (p2 +. p2 > p2) then p else *)
  sin_sub1 x (p *. 2.0) in
let rec sin_sub2 x p =
  if x < pi *. 2.0 then x else
  let x = if x >= p then x -. p else x in
  sin_sub2 x (p *. 0.5) in
let rec sin x =
  let pi = 3.14159265358979323846264 in
  let pi2 = pi *. 2.0 in
  let pih = pi *. 0.5 in
  (* tan *)
  let s1 = x > 0.0 in
  let x0 = fabs x in

  (* if not x0 +. x0 > x0, x0 is nan/inf?
     if not (x0 +. x0 > x0) then x0 *. 0.0 else *)
  let p = sin_sub1 x0 pi2 in
  let x1 = sin_sub2 x0 p in
  let s2 = if x1 > pi then not s1 else s1 in
  let x2 = if x1 > pi then pi2 -. x1 else x1 in
  let x3 = if x2 > pih then pi -. x2 else x2 in
  let t = tan (x3 *. 0.5) in
  let ans = 2. *. t /. (1. +. t *. t) in
  if s2 then ans else fneg ans in



(* cos *)
let rec cos x = 
  let pih = pi *. 0.5 in
  sin (pih -. x) in

let rec mul10 x = x * 8 + x * 2 in

(* mul *)
let rec mul_sub a b =
  if b = 0 then 0
  else (
    let b_mod_2 = b - (b / 2) * 2 in
    if b_mod_2 = 0 then
      (mul_sub (a * 2) (b / 2))
    else
      (mul_sub (a * 2) (b / 2)) + a
   ) in
let rec mul a b =
  if b < 0 then
    mul_sub (-a) (-b)
  else
    mul_sub a b in


(* read_int *)
let read_int_ans = Array.create 1 0 in
let read_int_s = Array.create 1 0 in
let rec read_int_token in_token prev =
  let c = input_char () in
  let flg = 
    if c < 48 then true
    else if c > 57 then true
    else false in
  if flg then
    (if in_token then (if read_int_s.(0) = 1 then read_int_ans.(0) else (-read_int_ans.(0))) else read_int_token false c)
  else
    ((if read_int_s.(0) = 0 then
      (* prev == '-' *)
      (if prev = 45 then read_int_s.(0) <- (-1) else read_int_s.(0) <- (1))
    else
      ());
    read_int_ans.(0) <- mul10 read_int_ans.(0) + (c - 48);
    read_int_token true c) in
let rec read_int _ = 
  read_int_ans.(0) <- 0;
  read_int_s.(0) <- 0;
  read_int_token false 32 in

(* read_float *)
let read_float_i = Array.create 1 0 in
let read_float_f = Array.create 1 0 in
let read_float_exp = Array.create 1 1 in
let read_float_s = Array.create 1 0 in
let rec read_float_token1 in_token prev =
  let c = input_char () in
  let flg =
    if c < 48 then true
    else if c > 57 then true
    else false in
  if flg then
    (if in_token then c else read_float_token1 false c)
  else
    ((if read_float_s.(0) = 0 then
      (* prev == '-' *)
      (if prev = 45 then read_float_s.(0) <- (-1) else read_float_s.(0) <- (1))

    else
      ());
    read_float_i.(0) <- mul10 read_float_i.(0) + (c - 48);
    read_float_token1 true c) in
let rec read_float_token2 in_token =
  let c = input_char () in
  if c < 48 or c > 57 then
    ()
  else
    (read_float_f.(0) <- mul10 read_float_f.(0) + (c - 48);
     read_float_exp.(0) <- mul10 read_float_exp.(0);
     read_float_token2 true)
in
let rec read_float _ = 
  read_float_i.(0) <- 0;
  read_float_f.(0) <- 0;
  read_float_exp.(0) <- 1;
  read_float_s.(0) <- 0;
  let nextch = read_float_token1 false 32 in
  let ans =
    if nextch = 46 then (* nextch = '.' *)
      (read_float_token2 false;
      (float_of_int read_float_i.(0)) +. (float_of_int read_float_f.(0)) /. (float_of_int read_float_exp.(0)))
    else
      float_of_int read_float_i.(0) in
  if read_float_s.(0) = 1 then
    ans
  else
    -. ans in


(* div_binary_search *)
let rec div_binary_search a b left right =
	let mid = (left + right) / 2 in
	let x = mid * b in
	if right - left <= 1 then
		left
	else
		if x < a then
			div_binary_search a b mid right
		else if x = a then
			mid
		else
			div_binary_search a b left mid in


(* print_int *)
let rec print_int_10000 x flg =

    (* 1000の位を表示 *)
    let tx = div_binary_search x 1000 0 10 in
    let dx = tx * 1000 in
    let x = x - dx in
    let flg = 
      if tx <= 0 then
        (if flg then
          (print_char (48 + tx); true)
        else
          false)
      else
        (print_char (48 + tx); true) in

    (* 100の位を表示 *)
    let tx = div_binary_search x 100 0 10 in
    let dx = tx * 100 in
    let x = x - dx in
    let flg = 
      if tx <= 0 then
        (if flg then
          (print_char (48 + tx); true)
        else
          false)
      else
        (print_char (48 + tx); true) in

    (* 10の位を表示 *)
    let tx = div_binary_search x 10 0 10 in
    let dx = mul10 tx  in
    let x = x - dx in
    let flg = 
      if tx <= 0 then
        (if flg then
          (print_char (48 + tx); true)
        else
          false)
      else
        (print_char (48 + tx); true) in

    (* 1の位を表示 *)
    print_char (48 + x)
in

(* print_int div命令を使わない版 *)
let rec print_int x =
  if x < 0 then
    (print_char 45; print_int (-x))
  else
    if x < 10000 then
      print_int_10000 x false
    else

    (* 100000000の位を表示 *)
    let tx = div_binary_search x 100000000 0 3 in
    let dx = tx * 100000000 in
    let x = x - dx in
    let flg = 
      if tx <= 0 then false
      else (print_char (48 + tx); true) in

    (* 10000000の位を表示 *)
    let tx = div_binary_search x 10000000 0 10 in
    let dx = tx * 10000000 in
    let x = x - dx in
    let flg = 
      if tx <= 0 then
        (if flg then
          (print_char (48 + tx); true)
        else
          false)
      else
        (print_char (48 + tx); true) in

    (* 1000000の位を表示 *)
    let tx = div_binary_search x 1000000 0 10 in
    let dx = tx * 1000000 in
    let x = x - dx in
    let flg = 
      if tx <= 0 then
        (if flg then
          (print_char (48 + tx); true)
        else
          false)
      else
        (print_char (48 + tx); true) in

    (* 100000の位を表示 *)
    let tx = div_binary_search x 100000 0 10 in
    let dx = tx * 100000 in
    let x = x - dx in
    let flg = 
      if tx <= 0 then
        (if flg then
          (print_char (48 + tx); true)
        else
          false)
      else
        (print_char (48 + tx); true) in

    (* 10000の位を表示 *)
    let tx = div_binary_search x 10000 0 10 in
    let dx = tx * 10000 in
    let x = x - dx in
    let flg = 
      if tx <= 0 then
        (if flg then
          (print_char (48 + tx); true)
        else
          false)
      else
        (print_char (48 + tx); true) in
    print_int_10000 x flg
in

let rec mod a b =
  let d = div a b in
  a - (mul b d)
in

let abs x =
  if x < 0 then 0-x else x
in


let rec land x y =
  if x < 0 then
    if y < 0 then land_sub (-x) (-y) 1
    else land_sub (-x) y 0
  else
    if y < 0 then land_sub x (-y) 0
    else land_sub x y 0 in
