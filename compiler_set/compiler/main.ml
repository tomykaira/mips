let limit = ref 1000

(* 最適化処理をくりかえす *)
let rec iter n e = 
  Format.eprintf "iteration %d@." n;
  if n = 0 then e else
  let e' = Elim.f (ConstFold.f (Inline.f (Assoc.f (Beta.f e)))) in
  if e = e' then e else
  iter (n - 1) e'

(* デバッグフラグとデバッグ用関数 *)
let dbpa = ref false
let dbty = ref false
let dbkn = ref false
let dbal = ref false
let dbit = ref false
let dbcl = ref false
let dbvi = ref false
let dbsi = ref false
let dbra = ref false

let debsy f t = (if !f = true then Syntax.dbprint 0 t else ()); t
let debkn f t = (if !f = true then KNormal.dbprint 0 t else ()); t
let debcl f t =
  (if !f = true then match t with 
                       Closure.Prog (x, y) -> Printf.eprintf "Functions:\n%!"; List.iter Closure.dbprint2 x; Printf.eprintf "\nMain Program:\n%!"; Closure.dbprint 1 y
  else ());
  t
let debas f t = 
  (if !f = true then match t with 
                       Asm.Prog (x, y) -> Printf.eprintf "Functions:\n%!"; List.iter Asm.dbprint3 x; Printf.eprintf "\nMain Program:\n%!"; Asm.dbprint2 1 y
  else ());
  t

let parse_buf_exn lexbuf =
  try
    Parser.exp Lexer.token lexbuf
  with exn ->
    begin
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let cnum = curr.Lexing.pos_cnum in
      let tok = Lexing.lexeme lexbuf in
      failwith (Printf.sprintf "Parse error at %d:%d `%s'" line cnum tok)
    end

(* バッファをコンパイルしてチャンネルへ出力する *)
let lexbuf outchan l =
  Id.counter := 0;
  Typing.extenv := M.empty;
  Emit.f outchan
    (debas dbra (RegAlloc.f
       (debas dbsi (Simm.f
	  (debas dbvi (Virtual.f
	     (debcl dbcl (Closure.f
		(debkn dbit (iter !limit
		   (debkn dbal (Alpha.f
		      (debkn dbkn (KNormal.f
			 (debsy dbty (Typing.f
			    (debsy dbpa (parse_buf_exn l))))))))))))))))))


(* 文字列をコンパイルして標準出力に表示する *)
let string s = lexbuf stdout (Lexing.from_string s) 

(* ファイルをコンパイルしてファイルに出力する *)
let file f = 
  let inchan = open_in (f ^ ".ml") in
  let outchan = open_out (f ^ ".s") in
  try
    lexbuf outchan (Lexing.from_channel inchan);
    close_in inchan;
    close_out outchan;
  with e -> (close_in inchan; close_out outchan; raise e)

(* ここからコンパイラの実行が開始される *)
let () = 
  let files = ref [] in
  Arg.parse
    [("--inline", Arg.Int(fun i -> Inline.threshold := i), "maximum size of functions inlined");
     ("-iter", Arg.Int(fun i -> limit := i), "maximum number of optimizations iterated");
   ("-b", Arg.Set Global.bin, "maximum number of optimizations iterated");
   ("-dbParser", Arg.Set dbpa, "debug: print Syntax.t after parsing");
   ("-dbTyping", Arg.Set dbty, "debug: print Syntax.t after typing");
   ("-dbKNormal", Arg.Set dbkn, "debug: print KNormal.t after K normalization");
   ("-dbAlpha", Arg.Set dbal, "debug: print KNormal.t after alpha");
   ("-dbIter", Arg.Set dbit, "debug: print KNormal.t after iter");
   ("-dbClosure", Arg.Set dbcl, "debug: print Closure.t after closure");
   ("-dbVirtual", Arg.Set dbvi, "debug: print Closure.t after virtualize");
   ("-dbSimm", Arg.Set dbsi, "debug: print Closure.t after Simm");
   ("-dbRegAlloc", Arg.Set dbra, "debug: print Closure.t after RegAlloc")]
    (fun s -> files := !files @ [s])
    ("Mitou Min-Caml Compiler (C) Eijiro Sumii\n" ^
     Printf.sprintf "usage: %s [-inline m] [-iter n] ...filenames without \".ml\"..." Sys.argv.(0));
  List.iter
    (fun f -> ignore (file f))
    !files
