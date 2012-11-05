open Asm
open GraphColor

let counter = ref 0

type alloc_result = (* allocにおいてspillingがあったかどうかを表すデータ型 *)
  | Alloc of Id.t (* allocated register *)
  | Spill of Id.t (* spilled variable *)
let rec alloc dest cont regenv graph x t = 
  (* allocate a register or spill a variable *)
  assert (not (M.mem x regenv));
  if t = Type.Unit then Alloc("%g0") else (* [XX] ad hoc optimization *)
  if is_reg x then Alloc(x) else
  let free = fv cont in
  match M.find x graph with
  | Colored r ->
      (try
	let y = List.find
	    (fun x -> try M.find x regenv = r with Not_found -> false)
	    free in
	Spill(y)
      with Not_found -> Alloc(r))
  | Prefer prefer ->
      let all =
	match t with
	| Type.Unit -> ["%g0"] (* dummy *)
	| Type.Float -> List.tl (List.rev allfregs)@[reg_fsw]
	| _ -> List.tl (List.tl (List.rev allregs))@[reg_sw;reg_cl] in
      try
        let live = (* 生きているレジスタ *)
	  List.fold_left
            (fun live y ->
	      if is_reg y then S.add y live else
              try S.add (M.find y regenv) live
              with Not_found -> live)
            S.empty
            free in
	let r = (* そうでないレジスタを探す *)
	  List.find
            (fun r -> not (S.mem r live))
            (prefer @ all) in
	Alloc(r)
      with Not_found ->
	let y = (* 型の合うレジスタ変数を探す *)
	  List.find
            (fun y ->
	      not (is_reg y) &&
	      try List.mem (M.find y regenv) all
	      with Not_found -> false)
	    (List.rev free) in
	Spill(y)

(* auxiliary function for g and g'_and_restore *)
let add x r regenv =
  if is_reg x then (assert (x = r); regenv) else
  M.add x r regenv

(* auxiliary functions for g' *)
exception NoReg of Id.t * Type.t
let find x t regenv =
  if is_reg x then x else
  try M.find x regenv
  with Not_found -> raise (NoReg(x, t))

(* 命令列から,関数呼び出しから関数呼び出しまでの一直線の区間を切り出す関数
   ついでにpreferの情報も集めておく.
   1番目の返り値は関数呼び出しがあったかどうか,2番目は切り出された命令列, 
   3番目はpreferの情報,4番目は分岐のとりかた.1がtaken *)
let addm x y p =
  if is_reg x || x.[0] = '%' || (List.hd y).[0] = '%' then p else
  try M.add x (remove_and_uniq S.empty (y@M.find x p)) p
  with Not_found -> M.add x y p
let rec simple dest = function
  | Ans(exp) -> simple' dest exp
  | Let((x,t) as xt, exp, e) ->
      let (a1, e1', p1, b1) = simple' xt exp in
      if a1 then (a1, e1', p1, b1)
      else let (a2, e2', p2, b2) = simple dest e in
      (a2, concat e1' xt e2', M.fold addm p2 p1, b1@b2)
and simple' dest = function
  | IfEq(x,y,e1,e2) | IfLE(x,y,e1,e2) | IfLT(x,y,e1,e2) ->
      let (a1, e1', p1, b1) = simple dest e1 in
      let (a2, e2', p2, b2) = simple dest e2 in
      if M.cardinal p1 > M.cardinal p2 then (a1, seq(Add(x,y),e1'), M.fold addm p1 p2, 1::b1)
      else (a2, seq(Add(x,y),e2'), M.fold addm p2 p1, 0::b2)
  | IfFEq(x,y,e1,e2) | IfFLE(x,y,e1,e2) | IfFLT(x,y,e1,e2) ->
      let (a1, e1', p1, b1) = simple dest e1 in
      let (a2, e2', p2, b2) = simple dest e2 in
      if M.cardinal p1 > M.cardinal p2 then (a1, seq(FAdd(x,y),e1'), M.fold addm p1 p2, 1::b1)
      else (a2, seq(FAdd(x,y),e2'), M.fold addm p2 p1, 0::b2)
  | CallDir(Id.L("min_caml_print_char" | "min_caml_input_char" | "min_caml_read_char"),_,_) as exp -> (false, Ans(exp), M.empty, [])
  | CallCls(x,ys,zs) as c ->
      let rec f x y = match (x,y) with
	| ([],_) | (_,[]) -> []
	| (z::xs,w::ys) -> (z,[w])::f xs ys in
      let prefer = addm x [reg_cl] (List.fold_left (fun x (y, z) -> addm y z x) (List.fold_left (fun x (y, z) -> addm y z x) M.empty (f ys allregs)) (f zs allfregs)) in
      (true, Ans(c), prefer, [])
  | CallDir(_,ys,zs) as c ->
      let rec f x y = match (x,y) with
	| ([],_) | (_,[]) -> []
	| (z::xs,w::ys) -> (z,[w])::f xs ys in
      let prefer = List.fold_left (fun x (y, z) -> addm y z x) (List.fold_left (fun x (y, z) -> addm y z x) M.empty (f ys allregs)) (f zs allfregs) in
      (true, Ans(c), prefer, [])
  | (AddI(y,0) | FMov(y)) as x ->
      let p = addm (fst dest) [y] (addm y [fst dest] M.empty) in
      (false, Ans(x), p, [])
  | exp -> (false, Ans(exp), M.empty, [])


(* 命令列の関数呼び出しまでの区間にレジスタ割り当て *)
let rec gc dest cont regenv e =
  let cont' = concat e dest cont in
  let (_, e', prefer, br) = simple ("%g0",Type.Unit) cont' in
  let (gr1, gf1) = make e' in
  let (gr2, sr) = spill gr1 (List.length allregs) in
  let (gf2, sf) = spill gf1 (List.length allfregs) in
  let gr3 = color allregs regenv prefer gr2 sr in
  let gf3 = color allfregs regenv prefer gf2 sf in
  let graph = M.fold M.add gr3 gf3 in
  g dest cont regenv br graph e

(* 命令列のレジスタ割り当て. contは後続の命令列 *)
and g dest cont regenv br graph = function 
  | Ans(exp) ->
      let regenv' =
	try (match M.find (fst dest) graph with Colored c -> add (fst dest) c regenv | _ -> regenv)
	with Not_found -> regenv in
      let (e, regenv'') = g'_and_restore dest cont regenv' br graph exp in
      (e, regenv'')
  | Let((x, t) as xt, exp, e) ->
      let regenv = M.remove x regenv in
      counter := !counter + 1; if !counter mod 1000 = 0 then Format.eprintf ".%!";
      let cont' = concat e dest cont in
      let (e1', regenv1') = g'_and_restore xt cont' regenv br graph exp in
      let regenv1 = M.remove x regenv1' in
      let is_call = function
	| CallDir(Id.L("min_caml_print_char" | "min_caml_input_char" | "min_caml_read_char"), _, _) -> false
	| CallCls _ | CallDir _ -> true
	| _ -> false in
      let is_br = function
	| IfEq _ | IfLE _ | IfLT _ | IfFEq _ | IfFLE _ | IfFLT _ -> true
	| _ -> false in
      if is_br exp then
	let gr =
	  if M.mem x regenv1' then M.singleton x (Colored (M.find x regenv1'))
          else M.singleton x (Prefer []) in
	(match alloc dest cont' regenv1 gr x t with
        | Spill(y) ->
  	    let r = M.find y regenv1 in
	    let (e2', regenv2) = gc dest cont (add x r (M.remove y regenv1)) e in
	    let save =
	      try Save(M.find y regenv, y)
	      with Not_found -> Nop in	    
	    (seq(save, concat e1' (r, t) e2'), regenv2)
        | Alloc(r) ->
	    let (e2', regenv2) = gc dest cont (add x r regenv1) e in
	    (concat e1' (r, t) e2', regenv2))
      else if is_call exp then
	let (reg, regenv1'') =
	  match t with
	  | Type.Unit -> ("%g0", regenv1)
	  | Type.Float -> (fregs.(0), add x fregs.(0) regenv1)
	  | _ -> (regs.(0), add x regs.(0) regenv1) in
	let (e2', regenv2) = gc dest cont regenv1'' e in
	(concat e1' (reg, t) e2', regenv2)
      else 
	(match alloc dest cont' regenv1 graph x t with
        | Spill(y) ->
  	    let r = M.find y regenv1 in
	    let (e2', regenv2) = g dest cont (add x r (M.remove y regenv1)) br graph e in
	    let save =
	      try Save(M.find y regenv, y)
	      with Not_found -> Nop in	    
	    (seq(save, concat e1' (r, t) e2'), regenv2)
        | Alloc(r) ->
	    let (e2', regenv2) = g dest cont (add x r regenv1) br graph e in
	    (concat e1' (r, t) e2', regenv2))

(* 使用される変数をスタックからレジスタへRestore *)
and g'_and_restore dest cont regenv br graph exp = 
  try g' dest cont regenv br graph exp
  with NoReg(x, t) ->
    g dest cont regenv br graph (Let((x, t), Restore(x), Ans(exp)))
and g' dest cont regenv br graph = function (* 各命令のレジスタ割り当て *)
  | Nop | Int _ | Float _ | SetL _ | Comment _ | Restore _ as exp -> (Ans(exp), regenv)
  | Add(x, y) -> (Ans(Add(find x Type.Int regenv, find y Type.Int regenv)), regenv)
  | Sub(x, y) -> (Ans(Sub(find x Type.Int regenv, find y Type.Int regenv)), regenv)
  | Mul(x, y) -> (Ans(Mul(find x Type.Int regenv, find y Type.Int regenv)), regenv)
  | And(x, y) -> (Ans(And(find x Type.Int regenv, find y Type.Int regenv)), regenv)
  | Or (x, y) -> (Ans(Or (find x Type.Int regenv, find y Type.Int regenv)), regenv)
  | Nor(x, y) -> (Ans(Nor(find x Type.Int regenv, find y Type.Int regenv)), regenv)
  | Xor(x, y) -> (Ans(Xor(find x Type.Int regenv, find y Type.Int regenv)), regenv)

  | AddI(x, y) -> (Ans(AddI(find x Type.Int regenv, y)), regenv)
  | SubI(x, y) -> (Ans(SubI(find x Type.Int regenv, y)), regenv)
  | MulI(x, y) -> (Ans(MulI(find x Type.Int regenv, y)), regenv)
  | AndI(x, y) -> (Ans(AndI(find x Type.Int regenv, y)), regenv)
  | OrI (x, y) -> (Ans(OrI (find x Type.Int regenv, y)), regenv)
  | NorI(x, y) -> (Ans(NorI(find x Type.Int regenv, y)), regenv)
  | XorI(x, y) -> (Ans(XorI(find x Type.Int regenv, y)), regenv)

  | SllI(x, y) -> (Ans(SllI(find x Type.Int regenv, y)), regenv)
  | SraI(x, y) -> (Ans(SraI(find x Type.Int regenv, y)), regenv)
  | IMovF(x) -> (Ans(IMovF(find x Type.Int regenv)), regenv)
  | FMovI(x) -> (Ans(FMovI(find x Type.Float regenv)), regenv)

  | FMov(x) -> (Ans(FMov(find x Type.Float regenv)), regenv)
  | FNeg(x) -> (Ans(FNeg(find x Type.Float regenv)), regenv)
  | FAdd(x, y) -> (Ans(FAdd(find x Type.Float regenv, find y Type.Float regenv)), regenv)
  | FSub(x, y) -> (Ans(FSub(find x Type.Float regenv, find y Type.Float regenv)), regenv)
  | FMul(x, y) -> (Ans(FMul(find x Type.Float regenv, find y Type.Float regenv)), regenv)
  | FMulN(x, y) -> (Ans(FMulN(find x Type.Float regenv, find y Type.Float regenv)), regenv)
  | FDiv(x, y) -> (Ans(FDiv(find x Type.Float regenv, find y Type.Float regenv)), regenv)
  | FInv(x) -> (Ans(FInv(find x Type.Float regenv)), regenv)
  | FSqrt(x) -> (Ans(FSqrt(find x Type.Float regenv)), regenv)

  | LdI(x, y) -> (Ans(LdI(find x Type.Int regenv, y)), regenv)
  | StI(x, y, z) -> (Ans(StI(find x Type.Int regenv, find y Type.Int regenv, z)), regenv)
  | LdR(x, y) -> (Ans(LdR(find x Type.Int regenv, find y Type.Int regenv)), regenv)
  | FLdI(x, y) -> (Ans(FLdI(find x Type.Int regenv, y)), regenv)
  | FStI(x, y, z) -> (Ans(FStI(find x Type.Float regenv, find y Type.Int regenv, z)), regenv)
  | FLdR(x, y) -> (Ans(FLdR(find x Type.Int regenv, find y Type.Int regenv)), regenv)

  | IfEq(x, y, e1, e2) as exp -> g'_if dest cont regenv br graph exp (fun e1' e2' -> IfEq(find x Type.Int regenv, find y Type.Int regenv, e1', e2')) e1 e2
  | IfLT(x, y, e1, e2) as exp -> g'_if dest cont regenv br graph exp (fun e1' e2' -> IfLT(find x Type.Int regenv, find y Type.Int regenv, e1', e2')) e1 e2
  | IfLE(x, y, e1, e2) as exp -> g'_if dest cont regenv br graph exp (fun e1' e2' -> IfLE(find x Type.Int regenv, find y Type.Int regenv, e1', e2')) e1 e2
  | IfFEq(x, y, e1, e2) as exp -> g'_if dest cont regenv br graph exp (fun e1' e2' -> IfFEq(find x Type.Float regenv, find y Type.Float regenv, e1', e2')) e1 e2
  | IfFLT(x, y, e1, e2) as exp -> g'_if dest cont regenv br graph exp (fun e1' e2' -> IfFLT(find x Type.Float regenv, find y Type.Float regenv, e1', e2')) e1 e2
  | IfFLE(x, y, e1, e2) as exp -> g'_if dest cont regenv br graph exp (fun e1' e2' -> IfFLE(find x Type.Float regenv, find y Type.Float regenv, e1', e2')) e1 e2

  | CallCls(x, ys, zs) as exp -> g'_call dest cont regenv exp (fun ys zs -> CallCls(find x Type.Int regenv, ys, zs)) ys zs
  | CallDir(Id.L l, ys, zs) as exp ->
      (match l with
	(* コンパイラで出力するライブラリ関数。
	   emit.mlでインラインに展開され,退避不要 *)
      | "min_caml_print_char" | "min_caml_input_char"
      | "min_caml_read_char" ->
	  (Ans(CallDir(Id.L l,
		       (List.map (fun y -> find y Type.Int regenv) ys),
	               (List.map (fun z -> find z Type.Float regenv) zs))),
	   regenv)
      | _ -> g'_call dest cont regenv exp (fun ys zs -> CallDir(Id.L l, ys, zs)) ys zs)
  | _ -> assert false

and g'_if dest cont regenv br graph exp constr e1 e2 = (* ifのレジスタ割り当て *)
  let ((e1', regenv1), (e2', regenv2)) =
    if List.hd br = 1 then
      (g dest cont regenv (List.tl br) graph e1, gc dest cont regenv e2)
    else
      (gc dest cont regenv e1, g dest cont regenv (List.tl br) graph e2) in
  let is_sw s = s = reg_sw || s = reg_fsw in
  if not (M.mem (fst dest) regenv1 && M.mem (fst dest) regenv2) || is_sw (M.find (fst dest) regenv1) || is_sw (M.find (fst dest) regenv2) then
    let regenv' = (* 両方に共通のレジスタ変数だけ利用 *)
      List.fold_left
	(fun regenv' x ->
          try
	    if is_reg x then regenv' else
            let r1 = M.find x regenv1 in
            let r2 = M.find x regenv2 in
            if r1 <> r2 then regenv' else
	    add x r1 regenv'
          with Not_found -> regenv')
	M.empty
	(fv cont) in
    (List.fold_left
       (fun e x ->
	 if x = fst dest || not (M.mem x regenv) || M.mem x regenv' then e else
	 seq(Save(M.find x regenv, x), e))
       (Ans(constr e1' e2'))
       (fv cont)
       ,regenv')
  else    
    (* aがtrueなら1が,falseなら2が基準 *)
    let a = M.cardinal regenv1 > M.cardinal regenv2 in
    (* AとB両方にある(場所は問わない)変数だけ使う。A基準。 *)
    let add' a b = if List.mem a b then b else a::b in
    let (regenv', rm, fm) = 
      List.fold_left
	(fun (regenv', rm, fm) x ->
	  if is_reg x then (regenv', rm, fm) else
          try
	    let rA = M.find x (if a then regenv1 else regenv2) in
	    let rB = M.find x (if a then regenv2 else regenv1) in
	    if is_sw rA || is_sw rB then (regenv', rm, fm)
	    else if rA.[1] = 'r' then (add x rA regenv', add' (rB,rA) rm, fm)
	    else (add x rA regenv', rm, add' (rB,rA) fm)
	  with Not_found -> (regenv', rm, fm))
	(M.empty, [], []) 
        (fst dest::fv cont) in
    let rm' = List.rev (Emit.shuffle reg_sw rm) in
    let fm' = List.rev (Emit.shuffle reg_fsw fm) in
    let dreg = M.find (fst dest) regenv' in
    let mov p =
      if (M.find (fst dest) regenv').[1] = 'r' then AddI(p, 0) else FMov(p) in
    let move =
      List.fold_left
	(fun e (p, r) -> Let((r,Type.Int), AddI(p, 0), e))
	(Ans(mov dreg))
	rm' in
    let move =
      List.fold_left
	(fun e (p, fr) -> Let((fr,Type.Float), FMov(p), e))
	move
	fm' in
    let e1'' = if a then e1'
    else concat e1' (M.find (fst dest) regenv1, snd dest) move in
    let e2'' = if not a then e2'
    else concat e2' (M.find (fst dest) regenv2, snd dest) move in
    (List.fold_left
       (fun e x ->
	 if x = fst dest || not (M.mem x regenv) || M.mem x regenv' then e else
	 seq(Save(M.find x regenv, x), e)) (* そうでない変数は分岐直前にセーブ *)
       (Ans(constr e1'' e2''))
       (fv cont)
       ,regenv')

(* 関数呼び出しのレジスタ割り当て *)
and g'_call dest cont regenv exp constr ys zs = 
  (List.fold_left
     (fun e x ->
       if x = fst dest || not (M.mem x regenv) then e else
       seq(Save(M.find x regenv, x), e))
     (Ans(constr
	    (List.map (fun y -> find y Type.Int regenv) ys)
	    (List.map (fun z -> find z Type.Float regenv) zs)))
     (fv cont),
   M.empty)

let h { name = Id.L(x); args = ys; fargs = zs; body = e; ret = t } = (* 関数のレジスタ割り当て *)
  let regenv = add x reg_cl M.empty in
  let (i, arg_regs, regenv) =
    List.fold_left
      (fun (i, arg_regs, regenv) y ->
        let r = regs.(i) in
        (i + 1,
	 arg_regs @ [r],
	 (assert (not (is_reg y));
	  add y r regenv)))
      (0, [], regenv)
      ys in
  let (d, farg_regs, regenv) =
    List.fold_left
      (fun (d, farg_regs, regenv) z ->
        let fr = fregs.(d) in
        (d + 1,
	 farg_regs @ [fr],
	 (assert (not (is_reg z));
	  add z fr regenv)))
      (0, [], regenv)
      zs in
  let (a, b) =
    match t with
    | Type.Unit -> (Id.gentmp Type.Unit, fun a -> AddI(a,0))
    | Type.Float -> (fregs.(0), fun a-> FMov(a))
    | _ -> (regs.(0), fun a -> AddI(a, 0)) in
  let (e', regenv') = gc (a, t) (Ans(b a)) regenv e in
  { name = Id.L(x); args = arg_regs; fargs = farg_regs; body = e'; ret = t }

let f (Prog(fundefs, e)) = (* プログラム全体のレジスタ割り当て *)
  Format.eprintf "register allocation: may take some time%!";
  let fundefs' = List.map h fundefs in
  let e', regenv' =
    gc (Id.gentmp Type.Unit, Type.Unit) (Ans(Nop)) M.empty e in
  Format.eprintf "@.";
  Prog(fundefs', e')
