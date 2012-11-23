(* テスト用 *)
(*NOMINCAML let rec display y x c = *)
(*NOMINCAML   print_int y; *)
(*NOMINCAML   print_char 44; *)
(*NOMINCAML   print_int x; *)
(*NOMINCAML   print_char 44; *)
(*NOMINCAML   print_char c; *)
(*NOMINCAML   print_char 32 in *)
(*NOMINCAML let rec readkbd _ = *)
(*NOMINCAML   print_newline () in *)

let column_max = 80 in
let row_max = 30 in


(* 1行読み取り配列に格納し,行番号、列番号、長さを返す.
   先頭の空白類文字は読み飛ばし *)
let rec read_line_sub input n state =
  let c = input_char () in
  if c = 10 or n >= column_max then (input.(n) <- 0; n)
  else if state = 0 then
    if c < 33 or c > 126 then read_line_sub input n 0
    else (input.(n) <- c; read_line_sub input (n+1) 1)
  else (input.(n) <- c; read_line_sub input (n+1) 1) in
let rec read_line input  =
  let y = read_int () in
  if y < 0 then (y, 0, 0) else
  let x = read_int () in
  let len = read_line_sub input 0 0 in
  (y, x, len)  in


(* 文字列,行番号,列番号,文字数を受け取り,出力 *)
let rec print_line_sub str y x n i =
  if i >= n then ()
  else
    let x2 = x+i in
    let c = str.(i) in
    if c = 0 then ()
    else display y x2 c; print_line_sub str y x n (i+1) in
let rec print_line str y x n =
  print_line_sub str y x n 0 in


let str = Array.create column_max 0 in
(* プログラムのメインループ.-1でページ切り替え,-2で終了 *)
let rec main _ =
  let (y, x, len) = read_line str in
  if y = -2 then ()
  else if y = -1 then (readkbd (); clear_display (); main ())
  else (print_line str y x len; main ()) in

main ()
