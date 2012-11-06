let limit = ref 1000

(* デバッグフラグとデバッグ用関数 *)
let dbpa = ref false
let dbty = ref false
let dbkn = ref false
let dbal = ref false
let dbit = ref false
let dbll = ref false
let dbcl = ref false
let dbft = ref false
let dbut = ref false
let dbet = ref false
let dba2 = ref false
let dblt = ref false
let dbvi = ref false
let dbc2 = ref false
let dbsi = ref false
let dbto = ref false
let dbra = ref false

let debsy f t = (if !f = true then Syntax.dbprint 0 t else ()); t
let debkn f t = (if !f = true then KNormal.dbprint 0 t else ()); t
let debcl f = function
    Closure.Prog (x, y) as t ->
      (if !f then (Printf.eprintf "Functions:\n%!"; List.iter Closure.dbprint2 x; Printf.eprintf "\nMain Program:\n%!"; Closure.dbprint 1 y));
      t
let debas f = function
    Asm.Prog (x, y) as t->
      (if !f then (Printf.eprintf "Functions:\n%!"; List.iter Asm.dbprint3 x; Printf.eprintf "\nMain Program:\n%!"; Asm.dbprint2 1 y));
      t


(* 特定の最適化処理をしないフラグとそれ用の関数 *)
let offcs = ref false
let offbe = ref false
let offas = ref false
let offin = ref false
let offcf = ref false
let offel = ref false
let offll = ref false
let offut = ref true
let offet = Global.offet := true; Global.offet
let offa2 = ref false
let offlt = ref false
let offc2 = ref true
let offsi = ref false
let offto = ref false

let off flag f x = if !flag then x else f x



(* 最適化処理をくりかえす *)
let rec iter n e = 
  Format.eprintf "iteration %d@." n;
  if n = 0 then e else
  let e' = off offcs Elim.f (off offcf ConstFold.f (off offin Inline.f (off offas Assoc.f (off offbe Beta.f (off offcs Cse.f e))))) in
  Format.eprintf "@.";
  if e = e' then e else
  iter (n - 1) e'


(* バッファをコンパイルしてチャンネルへ出力する *)
let lexbuf outchan l =
  Id.counter := 0;
  Typing.extenv := M.empty;
  Out.f outchan
    (JumpElim.f
     (Emit.f
      (debas dbra (RegAlloc.f
       (debas dbto (off offto Together.f
        (debas dbsi (off offsi Simm.f
         (debas dbc2 (off offc2 ConstFold2.f
 	  (debas dbvi (Virtual.f
 	   (debcl dblt (off offlt ElimTuple.f
	    (debcl dba2 (off offa2 Assoc2.f
	     (debcl dbet (off offet EmbedTuple.f
	      (debcl dbut (off offut UnfoldTuple.f
	       (debcl dbft (off (ref (!offet && !offut)) FlattenTuple.f
	        (debcl dbcl (Closure.f
	         (debkn dbll (off offll LambdaLift.f
	          (debkn dbit (iter !limit
	           (debkn dbal (Alpha.f
	            (debkn dbkn (KNormal.f
	             (debsy dbty (Typing.f
	              (debsy dbpa (Parser.exp Lexer.token l))))))))))))))))))))))))))))))))))))


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
   ("-b", Arg.Set Global.bin, "use binary file as input");

   ("-dbParser", Arg.Set dbpa, "debug: print Syntax.t after parsing");
   ("-dbTyping", Arg.Set dbty, "debug: print Syntax.t after typing");
   ("-dbKNormal", Arg.Set dbkn, "debug: print KNormal.t after K normalization");
   ("-dbAlpha", Arg.Set dbal, "debug: print KNormal.t after alpha");
   ("-dbIter", Arg.Set dbit, "debug: print KNormal.t after iter");
   ("-dbLambdaLift", Arg.Set dbll, "debug: print KNormal.t after LambdaLift");
   ("-dbClosure", Arg.Set dbcl, "debug: print Closure.t after closure");
   ("-dbFlattenTuple", Arg.Set dbft, "debug: print Closure.t after FT");
   ("-dbUnfoldTuple", Arg.Set dbut, "debug: print Closure.t after UT");
   ("-dbEmbedTuple", Arg.Set dbet, "debug: print Closure.t after EmbedTuple");
   ("-dbAssoc2", Arg.Set dba2, "debug: print Closure.t after Assoc2");
   ("-dbElimTuple", Arg.Set dblt, "debug: print Closure.t after ElimTuple");
   ("-dbVirtual", Arg.Set dbvi, "debug: print Asm.t after virtualize");
   ("-dbConstFold2", Arg.Set dbc2, "debug: print Asm.t after CF2");
   ("-dbSimm", Arg.Set dbsi, "debug: print Asm.t after Simm");
   ("-dbTogether", Arg.Set dbto, "debug: print Asm.t after Together");
   ("-dbRegAlloc", Arg.Set dbra, "debug: print Asm.t after RegAlloc");

   ("-offCSE", Arg.Set offcs, "off: NO CSE");
   ("-offBeta", Arg.Set offbe, "off: NO beta");
   ("-offAssoc", Arg.Set offas, "off: NO Assoc");
   ("-offInline", Arg.Set offin, "off: NO Inline");
   ("-offConstFold", Arg.Set offcf, "off: NO ConstFold");
   ("-offElim", Arg.Set offel, "off: NO Elim");
   ("-offLambdaLift", Arg.Set offll, "off: NO LambdaLift");
   ("-offUnfoldTuple", Arg.Set offut, "off: NO Unfold Tuple");
   ("-offEmbedTuple", Arg.Set offet, "off: NO Embed Tuple");
   ("-offAssoc2", Arg.Set offa2, "off: NO Assoc 2");
   ("-offElimTuple", Arg.Set offlt, "off: NO Elim Tuple");
   ("-offConstFold2", Arg.Set offc2, "off: NO ConstFold2");
   ("-offSimm", Arg.Set offsi, "off: NO Simm");
   ("-offTogether", Arg.Set offto, "off: NO Together");]
    (fun s -> files := !files @ [s])
    ("Mitou Min-Caml Compiler (C) Eijiro Sumii\n" ^
     Printf.sprintf "usage: %s [-inline m] [-iter n] ...filenames without \".ml\"..." Sys.argv.(0));
  List.iter
    (fun f -> ignore (file f))
    !files
