(* / 2, * 2はparser.mlyで左・右シフトに変換されるので使ってよい *)
let rec mul_sub a b =
  if a = 0 or b = 0 then 0
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

let rec div_sub a b left =
  if mul (b * 2) left  <= a then
    div_sub a b (left * 2)
  else
    div_binary_search a b left (left * 2) in

let rec div a b =
  (* a が大きすぎると overflow して止まらなくなる *)
  let (a, b) = if a > 1073741823 then (a/2, b/2) else (a, b) in
  (* bは0ではない *)
  let abs_a = if a >= 0 then a else -a in
  let abs_b = if b >= 0 then b else -b in
  if abs_a < abs_b then
    0
  else (
    let ans = div_sub abs_a abs_b 1 in
    if a >= 0 then (
      if b >= 0 then
        ans
      else
        -ans
    )
    else (
      if b >= 0 then
        -ans
      else
        ans
    )
  ) in
print_int (div (2 * 962158371) 1024)
