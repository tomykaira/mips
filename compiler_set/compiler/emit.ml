open Asm

external gethi : float -> int32 = "gethi"
external getlo : float -> int32 = "getlo"

let stackset = ref S.empty (* すでにSaveされた変数の集合 *)
let stackmap = ref [] (* Saveされた変数の、スタックにおける位置 *)
let save x =
  stackset := S.add x !stackset;
  if not (List.mem x !stackmap) then
    stackmap := !stackmap @ [x]
let savef x =
  stackset := S.add x !stackset;
  if not (List.mem x !stackmap) then
    (let pad =
      if List.length !stackmap mod 2 = 0 then [] else [Id.gentmp Type.Int] in
    stackmap := !stackmap @ pad @ [x; x])
let locate x = (* xがスタックのどこにあるか *)
  let rec loc = function
    | [] -> []
    | y :: zs when x = y -> 0 :: List.map succ (loc zs)
    | y :: zs -> List.map succ (loc zs) in
  loc !stackmap
let offset x = List.hd (locate x)
let stacksize () = List.length !stackmap + 1


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
let rec g oc = function (* 命令列のアセンブリ生成 *)
  | dest, Ans(exp) -> g' oc (dest, exp)
  | dest, Let((x, t), exp, e) ->
      g' oc (NonTail(x), exp);
      g oc (dest, e)
and g' oc = function (* 各命令のアセンブリ生成 *)
  (* 末尾でなかったら計算結果をdestにセット *)
  | NonTail(_), Nop -> ()

  | NonTail(x), Add(y, z) -> Printf.fprintf oc "\tadd\t%s, %s, %s\n" x y z
  | NonTail(x), Sub(y, z) -> Printf.fprintf oc "\tsub\t%s, %s, %s\n" x y z
  | NonTail(x), Mul(y, z) -> Printf.fprintf oc "\tmul\t%s, %s, %s\n" x y z
  | NonTail(x), And(y, z) -> Printf.fprintf oc "\tand\t%s, %s, %s\n" x y z
  | NonTail(x), Or (y, z) -> Printf.fprintf oc "\tor\t%s, %s, %s\n" x y z
  | NonTail(x), Nor(y, z) -> Printf.fprintf oc "\tnor\t%s, %s, %s\n" x y z
  | NonTail(x), Xor(y, z) -> Printf.fprintf oc "\txor\t%s, %s, %s\n" x y z

  | NonTail(x), AddI(y, z) when x = y && z = 0-> ()
  | NonTail(x), AddI(y, z) -> Printf.fprintf oc "\taddi\t%s, %s, %d\n" x y z
  | NonTail(x), SubI(y, z) -> Printf.fprintf oc "\tsubi\t%s, %s, %d\n" x y z
  | NonTail(x), MulI(y, z) -> Printf.fprintf oc "\tmuli\t%s, %s, %d\n" x y z
  | NonTail(x), AndI(y, z) -> Printf.fprintf oc "\tandi\t%s, %s, %d\n" x y z
  | NonTail(x), OrI (y, z) -> Printf.fprintf oc "\tori\t%s, %s, %d\n" x y z
  | NonTail(x), NorI(y, z) -> Printf.fprintf oc "\tnori\t%s, %s, %d\n" x y z
  | NonTail(x), XorI(y, z) -> Printf.fprintf oc "\txori\t%s, %s, %d\n" x y z

  | NonTail(x), Int(i) when -0x8000 <= i && i <= 0x7FFF ->
      Printf.fprintf oc "\taddi\t%s, %s, %d\n" x reg_0 i
  | NonTail(x), Int(i) ->
      Printf.fprintf oc "\tmvlo\t%s, %d\n\tmvhi\t%s, %d\n"
	x (i land 0xFFFF)
	x (Int32.to_int (Int32.logand (Int32.shift_right_logical (Int32.of_int i) 16) (Int32.of_int 0xFFFF)))
  | NonTail(x), Float(i) ->
      Printf.fprintf oc "\tfmvlo\t%s, %d\n\tfmvhi\t%s, %d\n"
	x (Int32.to_int (Int32. logand (Int32.bits_of_float i) (Int32.of_int 0xFFFF)))
	x (Int32.to_int (Int32.logand (Int32.shift_right_logical (Int32.bits_of_float i) 16) (Int32.of_int 0xFFFF)))
  | NonTail(x), SetL(Id.L(y)) -> Printf.fprintf oc "\tsetl\t%s, %s\n" x y
  | NonTail(x), SllI(y, z) -> Printf.fprintf oc "\tslli\t%s, %s, %d\n" x y z
  | NonTail(x), SraI(y, z) -> Printf.fprintf oc "\tsrai\t%s, %s, %d\n" x y z
  | NonTail(x), IMovF(y) -> Printf.fprintf oc "\timovf\t%s, %s\n" x y
  | NonTail(x), FMovI(y) -> Printf.fprintf oc "\tfmovi\t%s, %s\n" x y

  | NonTail(x), FMov(y) when x = y -> ()
  | NonTail(x), FMov(y) -> Printf.fprintf oc "\tfmov\t%s, %s\n" x y
  | NonTail(x), FNeg(y) -> Printf.fprintf oc "\tfneg\t%s, %s\n" x y
  | NonTail(x), FAdd(y, z) -> Printf.fprintf oc "\tfadd\t%s, %s, %s\n" x y z
  | NonTail(x), FSub(y, z) -> Printf.fprintf oc "\tfsub\t%s, %s, %s\n" x y z
  | NonTail(x), FMul(y, z) -> Printf.fprintf oc "\tfmul\t%s, %s, %s\n" x y z
  | NonTail(x), FMulN(y, z) -> Printf.fprintf oc "\tfmuln\t%s, %s, %s\n" x y z
  | NonTail(x), FDiv(y, z) -> Printf.fprintf oc "\tfinv\t%s, %s\n\tfmul %s, %s, %s\n" z z x y z
  | NonTail(x), FInv(y) -> Printf.fprintf oc "\tfinv\t%s, %s\n" x y
  | NonTail(x), FSqrt(y) -> Printf.fprintf oc "\tfsqrt\t%s, %s\n" x y

  | NonTail(x), LdI(y, z) -> Printf.fprintf oc "\tldi\t%s, %s, %d\n" x y z
  | NonTail(_), StI(x, y, z) -> Printf.fprintf oc "\tsti\t%s, %s, %d\n" x y z
  | NonTail(x), LdR(y, z) -> Printf.fprintf oc "\tldr\t%s, %s, %s\n" x y z
  | NonTail(x), FLdI(y, z) -> Printf.fprintf oc "\tfldi\t%s, %s, %d\n" x y z
  | NonTail(_), FStI(x, y, z) -> Printf.fprintf oc "\tfsti\t%s, %s, %d\n" x y z
  | NonTail(x), FLdR(y, z) -> Printf.fprintf oc "\tfldr\t%s, %s, %s\n" x y z

  | NonTail(_), Comment(s) -> Printf.fprintf oc "# %s\n" s

  (* 退避の仮想命令の実装 *)
  | NonTail(_), Save(x, y) when List.mem x allregs && not (S.mem y !stackset) ->
      save y;
      Printf.fprintf oc "\tsti\t%s, %s, %d\n" x reg_fp (-offset y)
  | NonTail(_), Save(x, y) when List.mem x allfregs && not (S.mem y !stackset) ->
      savef y;
      Printf.fprintf oc "\tfsti\t%s, %s, %d\n" x reg_fp (-offset y)
  | NonTail(_), Save(x, y) -> assert (S.mem y !stackset); ()
  (* 復帰の仮想命令の実装 *)
  | NonTail(x), Restore(y) when List.mem x allregs ->
      Printf.fprintf oc "\tldi\t%s, %s, %d\n" x reg_fp (-offset y)
  | NonTail(x), Restore(y) ->
      assert (List.mem x allfregs);
      Printf.fprintf oc "\tfldi\t%s, %s, %d\n" x reg_fp (-offset y)
  (* 末尾だったら計算結果を第一レジスタにセットしてret *)
  | Tail, (Nop | StI _ | FStI _ | Comment _ | Save _ as exp) ->
      g' oc (NonTail(Id.gentmp Type.Unit), exp);
      Printf.fprintf oc "\treturn\n"
  | Tail, (Add _ | Sub _ | Mul _ | And _ | Or _ | Nor _ | Xor _ | AddI _ | SubI _ | MulI _ | AndI _ | OrI _ | NorI _ | XorI _ | Int _ | SetL _ | SllI _ | SraI _ | FMovI _ | LdI _ | LdR _ as exp) ->
      g' oc (NonTail(regs.(0)), exp);
      Printf.fprintf oc "\treturn\n"
  | Tail, (Float _ | FMov _ | FNeg _ | FAdd _ | FSub _ | FMul _ | FMulN _ | FDiv _ | FInv _ | FSqrt _ | IMovF _ | FLdI _ | FLdR _  as exp) ->
      g' oc (NonTail(fregs.(0)), exp);
      Printf.fprintf oc "\treturn\n";
  | Tail, (Restore(x) as exp) ->
      let d = if x.[1] = 'r' then regs.(0) else fregs.(0) in
      (match locate x with
      | [i] -> g' oc (NonTail(d), exp)
      | _ -> assert false);
      Printf.fprintf oc "\treturn\n";

  | Tail, IfEq(x, y, e1, e2) ->
      Printf.fprintf oc "\tbeq\t%s, %s, " x y;
      g'_tail_if oc e1 e2 "beq"
  | Tail, IfLT(x, y, e1, e2) ->
      Printf.fprintf oc "\tblt\t%s, %s, " x y;
      g'_tail_if oc e1 e2 "blt"
  | Tail, IfLE(x, y, e1, e2) ->
      Printf.fprintf oc "\tble\t%s, %s, " x y;
      g'_tail_if oc e1 e2 "ble"
  | Tail, IfFEq(x, y, e1, e2) ->
      Printf.fprintf oc "\tfbeq\t%s, %s, " x y;
      g'_tail_if oc e1 e2 "fbe"
  | Tail, IfFLT(x, y, e1, e2) ->
      Printf.fprintf oc "\tfblt\t%s, %s, " x y;
      g'_tail_if oc e1 e2 "fblt"
  | Tail, IfFLE(x, y, e1, e2) ->
      Printf.fprintf oc "\tfble\t%s, %s, " x y;
      g'_tail_if oc e1 e2 "fble"

  | NonTail(z), IfEq(x, y, e1, e2) ->
      Printf.fprintf oc "\tbeq\t%s, %s, " x y;
      g'_non_tail_if oc (NonTail(z)) e1 e2 "beq"
  | NonTail(z), IfLT(x, y, e1, e2) ->
      Printf.fprintf oc "\tblt\t%s, %s, " x y;
      g'_non_tail_if oc (NonTail(z)) e1 e2 "blt"
  | NonTail(z), IfLE(x, y, e1, e2) ->
      Printf.fprintf oc "\tble\t%s, %s, " x y;
      g'_non_tail_if oc (NonTail(z)) e1 e2 "ble"
  | NonTail(z), IfFEq(x, y, e1, e2) ->
      Printf.fprintf oc "\tfbeq\t%s, %s, " x y;
      g'_non_tail_if oc (NonTail(z)) e1 e2 "fbe"
  | NonTail(z), IfFLT(x, y, e1, e2) ->
      Printf.fprintf oc "\tfblt\t%s, %s, " x y;
      g'_non_tail_if oc (NonTail(z)) e1 e2 "fblt"
  | NonTail(z), IfFLE(x, y, e1, e2) ->
      Printf.fprintf oc "\tfble\t%s, %s, " x y;
      g'_non_tail_if oc (NonTail(z)) e1 e2 "fble"

  (* 関数呼び出しの仮想命令の実装 *)
  | Tail, CallCls(x, ys, zs) -> (* 末尾呼び出し *)
      g'_args oc [(x, reg_cl)] ys zs;
      Printf.fprintf oc "\tldi\t%s, %s, 0\n" reg_sw reg_cl;
      Printf.fprintf oc "\tjr\t%s\n" reg_sw;
  | Tail, CallDir(Id.L(x), ys, zs) -> (* 末尾呼び出し *)
      g'_args oc [] ys zs;
      Printf.fprintf oc "\tj\t%s\n" x;

  | NonTail(a), CallCls(x, ys, zs) ->
      g'_args oc [(x, reg_cl)] ys zs;
      let ss = stacksize () in
      Printf.fprintf oc "\tsubi\t%s, %s, %d\n" reg_fp reg_fp ss;
      Printf.fprintf oc "\tldi\t%s, %s, 0\n" reg_sw reg_cl;
      Printf.fprintf oc "\tcallr\t%s\n" reg_sw;
      Printf.fprintf oc "\taddi\t%s, %s, %d\n" reg_fp reg_fp ss;
      if List.mem a allregs && a <> regs.(0) then
	Printf.fprintf oc "\taddi\t%s, %s, 0\n" a regs.(0)
      else if List.mem a allfregs && a <> fregs.(0) then
	Printf.fprintf oc "\tfmov\t%s, %s\n" a fregs.(0)
  | NonTail(a), CallDir(Id.L(x), ys, zs) ->
      g'_args oc [] ys zs;
      let ss = stacksize () in
      Printf.fprintf oc "\tsubi\t%s, %s, %d\n" reg_fp reg_fp ss;
      Printf.fprintf oc "\tcall\t%s\n" x;
      Printf.fprintf oc "\taddi\t%s, %s, %d\n" reg_fp reg_fp ss;
      if List.mem a allregs && a <> regs.(0) then
	Printf.fprintf oc "\taddi\t%s, %s, 0\n" a regs.(0)
      else if List.mem a allfregs && a <> fregs.(0) then
	Printf.fprintf oc "\tfmov\t%s, %s\n" a fregs.(0)

and g'_tail_if oc e1 e2 b =
  let b_taken = Id.genid (b ^ "_taken") in
  let stackset_back = !stackset in
  Printf.fprintf oc "%s\n" b_taken;
  g oc (Tail, e2);
  Printf.fprintf oc "%s:\n" b_taken;
  stackset := stackset_back;
  g oc (Tail, e1)
and g'_non_tail_if oc dest e1 e2 b =
  let b_taken = Id.genid (b ^ "_taken") in
  let b_cont = Id.genid (b ^ "_cont") in
  let stackset_back = !stackset in
  Printf.fprintf oc "%s\n" b_taken;
  g oc (dest, e2);
  let stackset1 = !stackset in
  Printf.fprintf oc "\tj\t%s\n" b_cont;
  Printf.fprintf oc "%s:\n" b_taken;
  stackset := stackset_back;
  g oc (dest, e1);
  Printf.fprintf oc "%s:\n" b_cont;
  let stackset2 = !stackset in
  stackset := S.inter stackset1 stackset2
and g'_args oc x_reg_cl ys zs =
  let (i, yrs) =
    List.fold_left
      (fun (i, yrs) y -> (i + 1, (y, regs.(i)) :: yrs))
      (0, x_reg_cl)
      ys in
  List.iter
    (fun (y, r) -> Printf.fprintf oc "\taddi\t%s, %s, 0\n" r y)
    (shuffle reg_sw yrs);
  let (d, zfrs) =
    List.fold_left
      (fun (d, zfrs) z -> (d + 1, (z, fregs.(d)) :: zfrs))
      (0, [])
      zs in
  List.iter
    (fun (z, fr) -> Printf.fprintf oc "\tfmov\t%s, %s\n" fr z)
    (shuffle reg_fsw zfrs)

let h oc { name = Id.L(x); args = _; fargs = _; body = e; ret = _ } =
  Printf.fprintf oc "%s:\n" x;
  stackset := S.empty;
  stackmap := [];
  g oc (Tail, e)

let f oc (Prog(fundefs, e)) =
  Format.eprintf "generating assembly...@.";
  Printf.fprintf oc "\tj min_caml_start\n";
  List.iter (fun fundef -> h oc fundef) fundefs;
  Printf.fprintf oc "min_caml_start:\n";
  Printf.fprintf oc "\taddi\t%s, %s, 1\n" reg_hp reg_0;
  Printf.fprintf oc "\tmvlo\t%s, 65535\n" reg_fp;
  Printf.fprintf oc "\tmvhi\t%s, 31\n" reg_fp;
  stackset := S.empty;
  stackmap := [];
  g oc (NonTail("$r0"), e);
  Printf.fprintf oc "\thalt\n"
