(*pp deriving *)

let limit = ref 1000


(* デバッグフラグとデバッグ用関数 *)
let dbpa = ref false
let dbty = ref false
let dbkn = ref false
let dbal = ref false
let dban = ref false
let dbit = ref false
let dbll = ref false
let dbcl = ref false
let dbft = ref false
let dbut = ref false
let dbet = ref false
let dblt = ref false
let dbvi = ref false
let dbsi = ref false
let dbto = ref false
let dbra = ref false


let debsy f t = (if !f = true then print_endline (Show.show<Syntax.t> t) else ()); t
let debkn f t = (if !f = true then print_endline (Show.show<KNormal.t> t) else ()); t
let deban f t = (if !f = true then print_endline (Show.show<ANormal.t> t) else ()); t
let debcl f t = (if !f = true then print_endline (Show.show<Closure.prog> t) else ()); t
let debas f t = (if !f = true then print_endline (Show.show<Asm.prog> t) else ()); t

(* 特定の最適化処理をしないフラグとそれ用の関数 *)
let offcs = ref false
let offbe = ref false
let offin = ref false
let offcf = ref false
let offte = ref false
let offel = ref false
let offaa = ref false
let offll = ref false
let offut = ref false
let offet = Global.offet := false; Global.offet
let offlt = ref false
let offsi = ref false
let offto = ref false
let offje = ref false
let offds = ref false

let off flag f x = if !flag then x else f x
let off2 flag f x = if flag then x else f x

(* 最適化処理をくりかえす *)
let rec iter n e = 
  Format.eprintf "iteration %d@." n;
  if n = 0 then e else
  let e' =  off offel Elim.f (off offte IfThenElse.f (off offcf ConstFold.f (off offin Inline.f (off2 (!offaa || (n mod 3 <> 1)) AliasAnalysis.f (off offbe Beta.f (off offcs Cse.f e)))))) in
  Format.eprintf "@.";
  if Beta.same M.empty e e' then e else
  iter (n - 1) e'


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
  Out.f outchan
    (off offds DelaySlotFilling.f
    (off offje JumpElim.f
     (Emit.f
      (debas dbra (RegAlloc.f
       (debas dbto (off offto Together.f
        (debas dbsi (off offsi Simm.f
 	  (debas dbvi (Virtual.f
		       (CollectDanger.f
 	    (debcl dblt (off offlt ElimTuple.f
	     (debcl dbet (off offet EmbedTuple.f
	      (debcl dbut (off offut UnfoldTuple.f
	       (debcl dbft (off (ref (!offet && !offut)) FlattenTuple.f
	        (debcl dbcl (Closure.f
	         (deban dbll (off offll LambdaLift.f
	          (deban dbit (iter !limit
        	   (deban dban (ANormal.f 
	            (debkn dbal (Alpha.f
	             (debkn dbkn (KNormal.f
	              (debsy dbty (Typing.f
	               (debsy dbpa (parse_buf_exn l))))))))))))))))))))))))))))))))))))



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
    [("-inline", Arg.Int(fun i -> Inline.threshold := i), "maximum size of functions inlined");
     ("-iter", Arg.Int(fun i -> limit := i), "maximum number of optimizations iterated");

   ("-b", Arg.Set Global.bin, "use binary file as input");
   ("-x", Arg.Set Global.emit_halt, "emit halt instead of the last return");

   ("-dbParser", Arg.Set dbpa, "debug: print Syntax.t after parsing");
   ("-dbTyping", Arg.Set dbty, "debug: print Syntax.t after typing");
   ("-dbKNormal", Arg.Set dbkn, "debug: print KNormal.t after K normalization");
   ("-dbAlpha", Arg.Set dbal, "debug: print KNormal.t after alpha");
   ("-dbANormal", Arg.Set dban, "debug: print ANormal.t after A normalizetion");
   ("-dbIter", Arg.Set dbit, "debug: print ANormal.t after iter");
   ("-dbLambdaLift", Arg.Set dbll, "debug: print ANormal.t after LambdaLift");
   ("-dbClosure", Arg.Set dbcl, "debug: print Closure.t after closure");
   ("-dbFlattenTuple", Arg.Set dbft, "debug: print Closure.t after FT");
   ("-dbUnfoldTuple", Arg.Set dbut, "debug: print Closure.t after UT");
   ("-dbEmbedTuple", Arg.Set dbet, "debug: print Closure.t after EmbedTuple");
   ("-dbElimTuple", Arg.Set dblt, "debug: print Closure.t after ElimTuple");
   ("-dbVirtual", Arg.Set dbvi, "debug: print Asm.t after virtualize");
   ("-dbSimm", Arg.Set dbsi, "debug: print Asm.t after Simm");
   ("-dbTogether", Arg.Set dbto, "debug: print Asm.t after Together");
   ("-dbRegAlloc", Arg.Set dbra, "debug: print Asm.t after RegAlloc");

   ("-offCSE", Arg.Set offcs, "off: NO CSE");
   ("-offBeta", Arg.Set offbe, "off: NO beta");
   ("-offInline", Arg.Set offin, "off: NO Inline");
   ("-offConstFold", Arg.Set offcf, "off: NO ConstFold");
   ("-offIfThenElse", Arg.Set offel, "off: NO if then else");
   ("-offElim", Arg.Set offel, "off: NO Elim");
   ("-offAliasAnalysis", Arg.Set offaa, "off: NO Alias Analysis");
   ("-offLambdaLift", Arg.Set offll, "off: NO LambdaLift");
   ("-offUnfoldTuple", Arg.Set offut, "off: NO Unfold Tuple");
   ("-offEmbedTuple", Arg.Set offet, "off: NO Embed Tuple");
   ("-offElimTuple", Arg.Set offlt, "off: NO Elim Tuple");
   ("-offSimm", Arg.Set offsi, "off: NO Simm");
   ("-offTogether", Arg.Set offto, "off: NO Together");
   ("-offJumpElim", Arg.Set offje, "off: NO jump elim");
   ("-offDelaySlotFilling", Arg.Set offds, "off: NO delay slot filling");]
    (fun s -> files := !files @ [s])
    ("Mitou Min-Caml Compiler (C) Eijiro Sumii\n" ^
        Printf.sprintf "usage: %s [-inline m] [-iter n] ...filenames without \".ml\"..." Sys.argv.(0));
  List.iter
    (fun f -> ignore (file f))
    !files
