(*pp deriving *)
open Util

let parse_buf_exn lexbuf =
  try
    Parser.translation_unit Lexer.token lexbuf
  with exn ->
    begin
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let cnum = curr.Lexing.pos_cnum in
      let tok = Lexing.lexeme lexbuf in
      failwith (Printf.sprintf "Parse error at %d:%d `%s'" line cnum tok)
    end

let notify str x = print_endline str; x

(* バッファをコンパイルしてチャンネルへ出力する *)
let lexbuf outchan l =
  Id.counter := 0;
  (
    Asm.print_all stdout
      $ (notify "print asm")
      $ GenerateAsm.convert
      $ (notify "generate asm")
      $ MemoryAllocation.convert
      $ (notify "memory allocation")
      $ RegisterAllocation.convert
      $ (notify "register allocation")
      $ Flow.convert
      $ (notify "flow")
      $ SimpleControl.convert
      $ (notify "simple control")
      $ FlatExp.convert
      $ (notify "flat exp")
      $ Alpha.convert
      $ (notify "alpha")
      $ MacroExpand.convert
      $ (notify "macro expand")
  ) (parse_buf_exn l)

(* ファイルをコンパイルしてファイルに出力する *)
let file f =
  let inchan = open_in f in
  let outchan = open_out ((BatString.rchop ~n:2 f) ^ ".s") in
  try
    lexbuf outchan (Lexing.from_channel inchan);
    close_in inchan;
    close_out outchan;
  with e -> (close_in inchan; close_out outchan; raise e)

(* ここからコンパイラの実行が開始される *)
let () =
  let files = ref [] in
  Arg.parse
    []
    (fun s -> files := !files @ [s])
    ("CCC (C) tomykaira 2012");
  List.iter
    (fun f -> ignore (file f))
    !files
