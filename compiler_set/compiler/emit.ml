open Asm

(* 結果を書き出す *)
let buf = ref []

let stackset = ref S.empty (* すでにSaveされた変数の集合 *)
let stackmap = ref [] (* Saveされた変数の、スタックにおける位置 *)
let save x =
  stackset := S.add x !stackset;
  if not (List.mem x !stackmap) then
    stackmap := !stackmap @ [x]
let offset x = (* xがスタックのどこにあるか *)
  let rec loc n = function
    | [] -> raise Not_found
    | y :: zs when x = y -> n
    | y :: zs -> loc (n+1) zs in
  loc 0 !stackmap
let stacksize () = List.length !stackmap

let rettuple = ref M.empty 


(* 関数呼び出しのために引数を並べ替える(register shuffling) *)
let rec shuffle sw xys =
  (* remove identical moves *)
  let _, xys = List.partition (fun (x, y) -> x = y) xys in
  (* find acyclic moves *)
  match List.partition (fun (_, y) -> List.mem_assoc y xys) xys with
  | [], [] -> []
  | (x, y) :: xys, [] -> (* no acyclic moves; resolve a cyclic move *)
      (y, sw) :: (x, y) :: shuffle sw (List.map
(function
| (y', z) when y = y' -> (sw, z)
| yz -> yz)
xys)
  | xys, acyc -> acyc @ shuffle sw xys

type dest = Tail | NonTail of Id.t (* 末尾かどうかを表すデータ型 *)
let rec g = function (* 命令列のアセンブリ生成 *)
  | dest, Ans(exp) -> g' (dest, exp)
  | dest, Let((x, t), exp, e) ->
      g' (NonTail(x), exp);
      g (dest, e)
and g' = function (* 各命令のアセンブリ生成 *)
    (* 末尾でなかったら計算結果をdestにセット *)
  | NonTail(_), Nop -> ()

  | NonTail(x), Add(y, z) -> Out.print buf (Out.Add(x,y,z))
  | NonTail(x), Sub(y, z) -> Out.print buf (Out.Sub(x,y,z))
  | NonTail(x), Mul(y, z) -> Out.print buf (Out.Mul(x,y,z))
  | NonTail(x), And(y, z) -> Out.print buf (Out.And(x,y,z))
  | NonTail(x), Or (y, z) -> Out.print buf (Out.Or(x,y,z))
  | NonTail(x), Nor(y, z) -> Out.print buf (Out.Nor(x,y,z))
  | NonTail(x), Xor(y, z) -> Out.print buf (Out.Xor(x,y,z))

  | NonTail(x), AddI(y, z) when x = y && z = 0-> ()
  | NonTail(x), AddI(y, z) -> Out.print buf (Out.AddI(x,y,z))
  | NonTail(x), SubI(y, z) -> Out.print buf (Out.SubI(x,y,z))
  | NonTail(x), MulI(y, z) -> Out.print buf (Out.MulI(x,y,z))
  | NonTail(x), AndI(y, z) -> Out.print buf (Out.AndI(x,y,z))
  | NonTail(x), OrI (y, z) -> Out.print buf (Out.OrI (x,y,z))
  | NonTail(x), NorI(y, z) -> Out.print buf (Out.NorI(x,y,z))
  | NonTail(x), XorI(y, z) -> Out.print buf (Out.XorI(x,y,z))

  | NonTail(x), Int(i) when -0x8000 <= i && i <= 0x7FFF ->
      Out.print buf (Out.AddI(x, reg_0, i))
  | NonTail(x), Int(i) ->
      Out.print buf (Out.Mvlo(x, Int32.to_int (Int32.logand (Int32.of_int i) (Int32.of_int 0xFFFF))));
      Out.print buf (Out.Mvhi(x, Int32.to_int (Int32.shift_right_logical (Int32.of_int i) 16)))
  | NonTail(x), Float(i) when i = 0.0 ->
      Out.print buf (Out.IMovF(x, reg_0))
  | NonTail(x), Float(i) ->
      Out.print buf (Out.FMvlo (x, Int32.to_int (Int32.logand (Int32.bits_of_float i) (Int32.of_int 0xFFFF))));
      Out.print buf (Out.FMvhi(x, Int32.to_int (Int32.shift_right_logical (Int32.bits_of_float i) 16)))

  | NonTail(x), SetL(Id.L(y)) -> Out.print buf (Out.SetL(x, y))
  | NonTail(x), SllI(y, z) -> Out.print buf (Out.SllI(x, y, z))
  | NonTail(x), SraI(y, z) -> Out.print buf (Out.SraI(x, y, z))
  | NonTail(x), IMovF(y) -> Out.print buf (Out.IMovF(x, y))
  | NonTail(x), FMovI(y) -> Out.print buf (Out.FMovI(x, y))

  | NonTail(x), FMov(y) when x = y -> ()
  | NonTail(x), FMov(y) -> Out.print buf (Out.FMov(x, y))
  | NonTail(x), FNeg(y) -> Out.print buf (Out.FNeg(x, y))
  | NonTail(x), FAdd(y, z) -> Out.print buf (Out.FAdd(x,y,z))
  | NonTail(x), FSub(y, z) -> Out.print buf (Out.FSub(x,y,z))
  | NonTail(x), FMul(y, z) -> Out.print buf (Out.FMul(x,y,z))
  | NonTail(x), FMulN(y, z) -> Out.print buf (Out.FMulN(x,y,z))
  | NonTail(x), FDiv(y, z) -> Out.print buf (Out.FInv(reg_fsw, z));
      Out.print buf (Out.FMul(x, y, reg_fsw))
  | NonTail(x), FDivN(y, z) -> Out.print buf (Out.FInv(reg_fsw, z));
      Out.print buf (Out.FMulN(x, y, reg_fsw))
  | NonTail(x), FInv(y) -> Out.print buf (Out.FInv(x, y))
  | NonTail(x), FSqrt(y) -> Out.print buf (Out.FSqrt(x, y))

  | NonTail(x), LdI(y, z) -> Out.print buf (Out.LdI(x,y,z))
  | NonTail(_), StI(x, y, z) -> Out.print buf (Out.StI(x,y,z))
  | NonTail(x), LdR(y, z) -> Out.print buf (Out.LdR(x,y,z))
  | NonTail(x), FLdI(y, z) -> Out.print buf (Out.FLdI(x,y,z))
  | NonTail(_), FStI(x, y, z) -> Out.print buf (Out.FStI(x,y,z))
  | NonTail(x), FLdR(y, z) -> Out.print buf (Out.FLdR(x,y,z))

  | NonTail(_), Comment(s) -> Out.print buf (Out.Comment s)

(* 退避の仮想命令の実装 *)
  | NonTail(_), Save(x, y) when List.mem x allregs && not (S.mem y !stackset) ->
      save y;
      Out.print buf (Out.StI(x, reg_fp, -offset y))
  | NonTail(_), Save(x, y) when List.mem x allfregs && not (S.mem y !stackset) ->
      save y;
      Out.print buf (Out.FStI(x, reg_fp, -offset y))
  | NonTail(_), Save(x, y) -> assert (S.mem y !stackset); ()
(* 復帰の仮想命令の実装 *)
  | NonTail(x), Restore(y) when List.mem x allregs ->
      Out.print buf (Out.LdI(x, reg_fp, -offset y))
  | NonTail(x), Restore(y) ->
      assert (List.mem x allfregs);
      Out.print buf (Out.FLdI(x, reg_fp, -offset y))
(* スタック領域の確保の仮想命令の実装 *)
  | NonTail(x), SAlloc(i) ->
      let x' = Id.genid "salloc" in
      let rec s n =
	if n <= 0 then []
	else (x' ^ "."^ string_of_int n)::s (n-1) in
      stackmap := !stackmap@s i;
      Out.print buf (Out.SubI(x, reg_fp, stacksize ()))

(* 末尾だったら計算結果を第一レジスタにセットしてret *)
  | Tail, (Nop | StI _ | FStI _ | Comment _ | Save _ as exp) ->
      g' (NonTail(Id.gentmp Type.Unit), exp);
      Out.print buf Out.Return
  | Tail, (Add _ | Sub _ | Mul _ | And _ | Or _ | Nor _ | Xor _ | AddI _ | SubI _ | MulI _ | AndI _ | OrI _ | NorI _ | XorI _ | Int _ | SetL _ | SllI _ | SraI _ | FMovI _ | LdI _ | LdR _ | SAlloc _ as exp) ->
      g' (NonTail(regs.(0)), exp);
      Out.print buf Out.Return
  | Tail, (Float _ | FMov _ | FNeg _ | FAdd _ | FSub _ | FMul _ | FMulN _ | FDiv _ | FDivN _ | FInv _ | FSqrt _ | IMovF _ | FLdI _ | FLdR _ as exp) ->
      g' (NonTail(fregs.(0)), exp);
      Out.print buf Out.Return
  | Tail, (Restore(x) as exp) ->
      let d = if x.[1] = 'r' then regs.(0) else fregs.(0) in
      g' (NonTail(d), exp);
      Out.print buf Out.Return

  | Tail, IfEq(x, y, e1, e2) ->
      let b_taken = Id.genid "beq_taken" in
      Out.print buf (Out.BEq(x, y, b_taken));
      g'_tail_if e1 e2 "beq" b_taken
  | Tail, IfLT(x, y, e1, e2) ->
      if e1 = Ans(Nop) && e2 = Ans(Nop) then () else
      if e2 = Ans(Nop) then g' (Tail, IfLE(y,x,e2,e1)) else
      let b_taken = Id.genid "blt_taken" in
      Out.print buf (Out.BLT(x, y, b_taken));
      g'_tail_if e1 e2 "blt" b_taken
  | Tail, IfLE(x, y, e1, e2) ->
      if e1 = Ans(Nop) && e2 = Ans(Nop) then () else
      if e2 = Ans(Nop) then g' (Tail, IfLT(y,x,e2,e1)) else
      let b_taken = Id.genid "ble_taken" in
      Out.print buf (Out.BLE(x, y, b_taken));
      g'_tail_if e1 e2 "ble" b_taken
  | Tail, IfFEq(x, y, e1, e2) ->
      let b_taken = Id.genid "fbeq_taken" in
      Out.print buf (Out.FBEq(x, y, b_taken));
      g'_tail_if e1 e2 "fbeq" b_taken
  | Tail, IfFLT(x, y, e1, e2) ->
      if e1 = Ans(Nop) && e2 = Ans(Nop) then () else
      if e2 = Ans(Nop) then g' (Tail, IfFLE(y,x,e2,e1)) else
      let b_taken = Id.genid "fblt_taken" in
      Out.print buf (Out.FBLT(x, y, b_taken));
      g'_tail_if e1 e2 "fblt" b_taken
  | Tail, IfFLE(x, y, e1, e2) ->
      if e1 = Ans(Nop) && e2 = Ans(Nop) then () else
      if e2 = Ans(Nop) then g' (Tail, IfFLT(y,x,e2,e1)) else
      let b_taken = Id.genid "fble_taken" in
      Out.print buf (Out.FBLE(x, y, b_taken));
      g'_tail_if e1 e2 "fble" b_taken

  | NonTail(z), IfEq(x, y, e1, e2) ->
      let b_taken = Id.genid "beq_taken" in
      Out.print buf (Out.BEq(x, y, b_taken));
      g'_non_tail_if (NonTail(z)) e1 e2 "beq" b_taken
  | NonTail(z), IfLT(x, y, e1, e2) ->
      if e1 = Ans(Nop) && e2 = Ans(Nop) then () else
      if e2 = Ans(Nop) then g' (NonTail(z), IfLE(y,x,e2,e1)) else
      let b_taken = Id.genid "blt_taken" in
      Out.print buf (Out.BLT(x, y, b_taken));
      g'_non_tail_if (NonTail(z)) e1 e2 "blt" b_taken
  | NonTail(z), IfLE(x, y, e1, e2) ->
      if e1 = Ans(Nop) && e2 = Ans(Nop) then () else
      if e2 = Ans(Nop) then g' (NonTail(z), IfLT(y,x,e2,e1)) else
      let b_taken = Id.genid "ble_taken" in
      Out.print buf (Out.BLE(x, y, b_taken));
      g'_non_tail_if (NonTail(z)) e1 e2 "ble" b_taken
  | NonTail(z), IfFEq(x, y, e1, e2) ->
      let b_taken = Id.genid "fbeq_taken" in
      Out.print buf (Out.FBEq(x, y, b_taken));
      g'_non_tail_if (NonTail(z)) e1 e2 "fbeq" b_taken
  | NonTail(z), IfFLT(x, y, e1, e2) ->
      if e1 = Ans(Nop) && e2 = Ans(Nop) then () else
      if e2 = Ans(Nop) then g' (NonTail(z), IfFLE(y,x,e2,e1)) else
      let b_taken = Id.genid "fblt_taken" in
      Out.print buf (Out.FBLT(x, y, b_taken));
      g'_non_tail_if (NonTail(z)) e1 e2 "fblt" b_taken
  | NonTail(z), IfFLE(x, y, e1, e2) ->
      if e1 = Ans(Nop) && e2 = Ans(Nop) then () else
      if e2 = Ans(Nop) then g' (NonTail(z), IfFLT(y,x,e2,e1)) else
      let b_taken = Id.genid "fble_taken" in
      Out.print buf (Out.FBLE(x, y, b_taken));
      g'_non_tail_if (NonTail(z)) e1 e2 "fble" b_taken

(* 関数呼び出しの仮想命令の実装 *)
  | Tail, CallCls(_, x, ys, zs) -> (* 末尾呼び出し *)
      g'_args [(x, reg_cl)] ys zs;
      Out.print buf (Out.LdI(reg_sw, reg_cl, 0));
      Out.print buf (Out.Jr(reg_sw))
  | Tail, CallDir(Id.L(x), ys, zs) -> (* 末尾呼び出し *)
      (match x with
(* コンパイラでインラインに出力されるライブラリ関数 *)
      | "min_caml_print_char" ->
	  Out.print buf (Out.Outputb(List.hd ys));
	  Out.print buf Out.Return
      | "min_caml_input_char" | "min_caml_read_char" ->
	  Out.print buf (Out.Inputb(regs.(0)));
	  Out.print buf Out.Return
      | _ -> g'_args [] ys zs;
          Out.print buf (Out.J x))

  | NonTail(a), CallCls(Id.L(l),x, ys, zs) ->
      g'_args [(x, reg_cl)] ys zs;

      let inc = if M.mem l !rettuple && not (S.mem l !CollectDanger.danger) then M.find l !rettuple else 0 in
      let x' = Id.genid "salloc" in
      let rec s n =
	if n <= 0 then []
	else (x' ^ "."^ string_of_int n)::s (n-1) in
      stackmap := !stackmap@s inc;
      let ss = stacksize () in
      if ss > 0 then Out.print buf (Out.SubI(reg_fp, reg_fp, ss));
      Out.print buf (Out.LdI(reg_sw, reg_cl, 0));
      Out.print buf (Out.Comment ("\tCall "^l));
      Out.print buf (Out.CallR(reg_sw));
      if ss > 0 then Out.print buf (Out.AddI(reg_fp, reg_fp, ss));
      if List.mem a allregs && a <> regs.(0) then
	Out.print buf (Out.AddI(a, regs.(0), 0))
      else if List.mem a allfregs && a <> fregs.(0) then
	Out.print buf (Out.FMov(a, fregs.(0)))
  | NonTail(a), CallDir(Id.L(x), ys, zs) ->
      (match x with
(* コンパイラでインラインに出力されるライブラリ関数 *)
      | "min_caml_print_char" ->
	  Out.print buf (Out.Outputb(List.hd ys))
      | "min_caml_input_char" | "min_caml_read_char" ->
	  Out.print buf (Out.Inputb(a))
      | _-> g'_args [] ys zs;
	  let inc = if M.mem x !rettuple && not (S.mem x !CollectDanger.danger) then M.find x !rettuple else 0 in
	  let x' = Id.genid "salloc" in
	  let rec s n =
	    if n <= 0 then []
	    else (x' ^ "."^ string_of_int n)::s (n-1) in
	  stackmap := !stackmap@s inc;
	  let ss = stacksize () in
	  if ss > 0 then Out.print buf (Out.SubI(reg_fp, reg_fp, ss));
	  Out.print buf (Out.Call x);
	  if ss > 0 then Out.print buf (Out.AddI(reg_fp, reg_fp, ss));
	  if List.mem a allregs && a <> regs.(0) then
	    Out.print buf (Out.AddI(a, regs.(0), 0))	
	  else if List.mem a allfregs && a <> fregs.(0) then
	    Out.print buf (Out.FMov(a, fregs.(0))))

and g'_tail_if e1 e2 b b_taken =
  let stackset_back = !stackset in
  Out.print buf Out.Nop; 
  g (Tail, e2);
  Out.print buf (Out.Label b_taken);
  stackset := stackset_back;
  g (Tail, e1)
and g'_non_tail_if dest e1 e2 b b_taken =
  let b_cont = Id.genid (b ^ "_cont") in
  let stackset_back = !stackset in
  Out.print buf Out.Nop; 
  g (dest, e2);
  let stackset1 = !stackset in
  Out.print buf (Out.J b_cont);
  Out.print buf (Out.Label b_taken);
  stackset := stackset_back;
  g (dest, e1);
  Out.print buf (Out.Label b_cont);
  let stackset2 = !stackset in
  stackset := S.inter stackset1 stackset2
and g'_args x_reg_cl ys zs =
  let (i, yrs) =
    List.fold_left
      (fun (i, yrs) y -> (i + 1, (y, regs.(i)) :: yrs))
      (0, x_reg_cl)
      ys in
  List.iter
    (fun (y, r) -> Out.print buf (Out.AddI(r, y, 0)))
    (shuffle reg_sw yrs);
  let (d, zfrs) =
    List.fold_left
      (fun (d, zfrs) z -> (d + 1, (z, fregs.(d)) :: zfrs))
      (0, [])
      zs in
  List.iter
    (fun (z, fr) -> Out.print buf (Out.FMov(fr, z)))
    (shuffle reg_fsw zfrs)

let h { name = Id.L(x); args = _; fargs = _; body = e; ret = t } =
  Out.print buf (Out.Label x);
  stackset := S.empty;
  stackmap := [];
  (match t with
    | Type.Tuple(ts) -> (rettuple := M.add x (List.length ts) !rettuple)
    | _ -> ());
  g (Tail, e);
  Out.print buf (Out.Comment "")

let f (Prog(fundefs, e)) =
  Format.eprintf "generating assembly...@.";
  Out.print buf (Out.J "min_caml_start");
  List.iter (fun fundef -> h fundef) fundefs;
  Out.print buf (Out.Label "min_caml_start");
  Out.print buf (Out.AddI(reg_hp, reg_0, 0));
  Out.print buf (Out.Mvlo(reg_fp, 65535));
  Out.print buf (Out.Mvhi(reg_fp, 2047));  (* 512MB *)
  Out.print buf (Out.OrI(reg_1, reg_0, 1));
  Out.print buf (Out.Nor(reg_m1, reg_0, reg_0));
  stackset := S.empty;
  stackmap := [];
  g (NonTail(reg_0), e);
  Out.print buf (Out.Comment "Send end marker, then halt");
  Out.print buf (Out.AddI("$r3", reg_0, 231));
  Out.print buf (Out.Outputb("$r3"));
  Out.print buf (Out.AddI("$r3", reg_0, 181));
  Out.print buf (Out.Outputb("$r3"));
  Out.print buf (Out.AddI("$r3", reg_0, 130));
  Out.print buf (Out.Outputb("$r3"));
  Out.print buf Out.Halt;
  List.rev !buf
