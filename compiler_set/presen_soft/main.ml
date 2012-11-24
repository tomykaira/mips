(* 1行読み取り配列に格納し,行番号、列番号、長さを返す.
   先頭の空白類文字は読み飛ばし *)
let rec read_line_sub input n state =
  let c = input_char () in
  if c = 10 then input.(n) <- 0; n
  else if n >= 80 then input.(n) <- 0; n
  else if state = 0 then 
    if c < 33 then read_line_sub input n 0
    else if c > 126 then read_line_sub input n 0
    else (input.(n) <- c; read_line_sub input (n+1) 1)
  else (input.(n) <- c; read_line_sub input (n+1) 1) in
let rec read_line _  =
  let y = read_int () in
  if y < 0 then (y, 0, 0, Array.create 0 0) else
  let x = read_int () in  
  let str = Array.create 80 0 in
  let len = read_line_sub str 0 0 in
  (y, x, len, str)  in

(* ファイルの読み込み.-1でページ切り替え *)
let rec read_page m page =
  if m > 24 then () else
  let (y, x, len, str) = read_line () in
  page.(m) <- (y, x, len, str);
  if y = -1 then ()
  else read_page (m+1) page in
let rec read_pages pn n pages =
  if n <= 0 then () else
  let str_dummy = Array.create 0 0 in
  pages.(pn-n) <- Array.create 24 (0, 0, 0, str_dummy);
  read_page 0 (pages.(pn-n));
  read_pages pn (n-1) pages in




(* 文字列,行番号,列番号,文字数を受け取り,出力 *)
let rec print_line_sub str y x n i =
  if i >= n then ()
  else
    let x2 = x+i in
    let c = str.(i) in
    if c = 0 then ()
    else (display y x2 c; print_line_sub str y x n (i+1)) in
let rec print_line str y x n =
  print_line_sub str y x n 0 in

(* 各ページの出力 *)
let rec print_page n page =
  if n > 24 then () else
  let (y, x, len, str) = page.(n) in
  if y < 0 then ()
  else (print_line str y x len; print_page (n+1) page) in
let rec print_pages pn n pages =
  if n <= 0 then ()
  else (print_page 0 pages.(pn-n); readkbd (); clear_display (); print_pages pn (n-1) pages) in


(* まずページ数を読み取る *)
let pn = read_int () in
(* 配列の確保 *)
let str_dummy = Array.create 0 0 in
let page_dummy = Array.create 0 (0, 0, 0, str_dummy) in
let pages = Array.create pn page_dummy in
(* 続いて各ページを読み取る *)
read_pages pn pn pages;
readkbd ();
print_pages pn pn pages
