open Asm
open GraphColor

let counter = ref 0

let pg0 = ("%g0", Type.Unit)



type alloc_result = (* allocにおいてspillingがあったかどうかを表すデータ型 *)
  | Alloc of Id.t (* allocated register *)
  | Spill of Id.t (* spilled variable *)
let rec alloc cont regenv graph x t = 
  (* allocate a register or spill a variable *)
  assert (not (M.mem x regenv));
  if t = Type.Unit then Alloc("%g0") else (* [XX] ad hoc optimization *)
  if is_reg x then Alloc(x) else
  let free = fv cont in
  let graph = if M.mem x graph then graph else M.add x (Prefer ([],[])) graph in
  match M.find x graph with
  | Colored r ->
      (try
	let y = List.find
	    (fun x -> try M.find x regenv = r with Not_found -> false)
	    free in
	Spill(y)
      with Not_found -> Alloc(r))
  | Prefer (prefer, hate) ->
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
	let (all2, all1) = List.partition (fun x -> List.mem x hate) all in
	let r = (* そうでないレジスタを探す *)
	  List.find
            (fun r -> not (S.mem r live))
            (prefer @ all1 @ all2) in
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



(* 式が関数呼び出しか判定 *)
let is_call = function
  | CallCls _ | CallDir _ -> true
  | _ -> false



(* 命令列から,関数呼び出しから関数呼び出しまでの一直線の区間を切り出す関数
   ついでにpreferの情報も集めておく.
   1番目の返り値は関数呼び出しがあったかどうか,2番目は切り出された命令列, 
   3番目はpreferの情報. *)

(* 命令列中に関数呼び出しが存在するか判定 *)
let rec ecall = function
  | Ans(exp) -> ecall' exp
  | Let(_, exp, e) -> ecall' exp || ecall e
and ecall' = function
  | IfEq(_,_,e1,e2) | IfLE(_,_,e1,e2) | IfLT(_,_,e1,e2) | IfFEq(_,_,e1,e2) | IfFLE(_,_,e1,e2) | IfFLT(_,_,e1,e2) -> ecall e1 || ecall e2
  | exp -> is_call exp
(* 本体 *)
let rec simple dest = function
  | Ans(exp) -> simple' pg0 (Ans(Nop)) exp
  | Let(xt, exp, e) ->
      let (a1, e1') = simple' xt e exp in
      if a1 then (a1, e1') else
      let (a2, e2') = simple dest e in
      (a2, concat e1' xt e2')
and simple' dest cont = function
  | IfEq(x,y,e1,e2) ->
      simple'_if dest cont (fun e1 e2 -> Ans(IfEq(x,y,e1,e2))) e1 e2
  | IfLE(x,y,e1,e2) ->
      simple'_if dest cont (fun e1 e2 -> Ans(IfLE(x,y,e1,e2))) e1 e2
  | IfLT(x,y,e1,e2) ->
      simple'_if dest cont (fun e1 e2 -> Ans(IfLT(x,y,e1,e2))) e1 e2
  | IfFEq(x,y,e1,e2) ->
      simple'_if dest cont (fun e1 e2 -> Ans(IfFEq(x,y,e1,e2))) e1 e2
  | IfFLE(x,y,e1,e2) ->
      simple'_if dest cont (fun e1 e2 -> Ans(IfFLE(x,y,e1,e2))) e1 e2
  | IfFLT(x,y,e1,e2) ->
      simple'_if dest cont (fun e1 e2 -> Ans(IfFLT(x,y,e1,e2))) e1 e2
  | (CallCls _ | CallDir _) as c -> (true, Ans(c))
  | exp -> (false, Ans(exp))
and simple'_if dest cont constr e1 e2 =
  let ((a1, e1'), (a2, e2')) =
    if dest = pg0 then (simple dest e1, simple dest e2) else
    if ecall e1 then
      if ecall e2 then (simple dest e1, simple dest e2)
      else (simple dest e1, simple pg0 (concat e2 dest cont))
    else
      if ecall e2 then (simple pg0 (concat e1 dest cont), simple dest e2)
      else (simple dest e1, simple dest e2) in
  (a1 || a2, constr e1' e2')


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
(* 命令列の関数呼び出しまでの区間にレジスタ割り当て *)
let rec gc dest cont regenv ifprefer e =
  let cont' = concat e dest cont in
  let (_, e') = simple pg0 cont' in
  let (gr1, gf1, prefer') = make e' in
  let prefer = if ecall e then prefer' else punion prefer' ifprefer in
  let (gr2, sr) = spill regenv prefer gr1 (List.length allregs) in
  let (gf2, sf) = spill regenv prefer gf1 (List.length allfregs) in
  let gr3 = color allregs regenv prefer gr2 sr in
  let gf3 = color allfregs regenv prefer gf2 sf in
  let graph = M.fold M.add gr3 gf3 in
  g dest cont regenv graph ifprefer e

(* 命令列のレジスタ割り当て. contは後続の命令列 *)
    and g dest cont regenv graph ifprefer = function 
      | Ans(exp) -> g'_and_restore dest cont regenv graph ifprefer exp 
      | Let((x, t) as xt, exp, e) ->
	  assert (not (M.mem x regenv));
	  counter := !counter + 1; if !counter mod 1000 = 0 then Format.eprintf ".%!";
	  let cont' = concat e dest cont in
	  let (e1', regenv1, graph1) = g'_and_restore xt cont' regenv graph ifprefer exp in
	  if is_br exp then
	    (match alloc cont' regenv1 graph1 x t with
            | Spill(y) ->
  		let r = try M.find y regenv1 with _ -> assert false in
		let (e2', regenv2, graph2) =
		  gc dest cont (add x r (M.remove y regenv1)) ifprefer e in
		let save =
		  try Save(M.find y regenv, y)
		  with Not_found -> Nop in
		(seq(save, concat e1' (r, t) e2'), regenv2, graph2)
            | Alloc(r) ->
		let (e2', regenv2, graph2) = gc dest cont (add x r regenv1) ifprefer e in
		(concat e1' (r, t) e2', regenv2, graph2))
	  else if is_call exp then
	    let (reg, regenv1'') =
	      match t with
	      | Type.Unit -> ("%g0", regenv1)
	      | Type.Float -> (fregs.(0), add x fregs.(0) regenv1)
	      | _ -> (regs.(0), add x regs.(0) regenv1) in
	    let (e2', regenv2, graph2) = gc dest cont regenv1'' ifprefer e in
	    (concat e1' (reg, t) e2', regenv2, graph2)
	  else
	    (match alloc cont' regenv1 graph x t with
            | Spill(y) ->
  		let r = M.find y regenv1 in
		let (e2', regenv2, graph2) =
		  g dest cont (add x r (M.remove y regenv1)) graph ifprefer e in
		let save =
		  try Save(M.find y regenv, y)
		  with Not_found -> Nop in	    
		(seq(save, concat e1' (r, t) e2'), regenv2, graph2)
            | Alloc(r) ->
		let (e2', regenv2, graph2) = g dest cont (add x r regenv1) graph ifprefer e in
		(concat e1' (r, t) e2', regenv2, graph2))

(* 使用される変数をスタックからレジスタへRestore *)
    and g'_and_restore dest cont regenv graph ifprefer exp = 
      try g' dest cont regenv graph ifprefer exp
      with NoReg(x, t) -> 
	g dest cont regenv graph ifprefer (Let((x, t), Restore(x), Ans(exp)))
    and g' dest cont regenv graph ifprefer = function (* 各命令のレジスタ割り当て *)
      | Nop | Int _ | Float _ | SetL _ | Comment _ | Restore _ | SAlloc _ | Inputb as exp -> (Ans(exp), regenv, graph)
      | Add(x, y) -> (Ans(Add(find x Type.Int regenv, find y Type.Int regenv)), regenv, graph)
      | Sub(x, y) -> (Ans(Sub(find x Type.Int regenv, find y Type.Int regenv)), regenv, graph)
      | Xor(x, y) -> (Ans(Xor(find x Type.Int regenv, find y Type.Int regenv)), regenv, graph)

      | AddI(x, y) -> (Ans(AddI(find x Type.Int regenv, y)), regenv, graph)
      | SubI(x, y) -> (Ans(SubI(find x Type.Int regenv, y)), regenv, graph)
      | XorI(x, y) -> (Ans(XorI(find x Type.Int regenv, y)), regenv, graph)

      | SllI(x, y) -> (Ans(SllI(find x Type.Int regenv, y)), regenv, graph)
      | SraI(x, y) -> (Ans(SraI(find x Type.Int regenv, y)), regenv, graph)
      | IMovF(x) -> (Ans(IMovF(find x Type.Int regenv)), regenv, graph)
      | FMovI(x) -> (Ans(FMovI(find x Type.Float regenv)), regenv, graph)

      | FMov(x) -> (Ans(FMov(find x Type.Float regenv)), regenv, graph)
      | FNeg(x) -> (Ans(FNeg(find x Type.Float regenv)), regenv, graph)
      | FAdd(x, y) -> (Ans(FAdd(find x Type.Float regenv, find y Type.Float regenv)), regenv, graph)
      | FSub(x, y) -> (Ans(FSub(find x Type.Float regenv, find y Type.Float regenv)), regenv, graph)
      | FMul(x, y) -> (Ans(FMul(find x Type.Float regenv, find y Type.Float regenv)), regenv, graph)
      | FMulN(x, y) -> (Ans(FMulN(find x Type.Float regenv, find y Type.Float regenv)), regenv, graph)
      | FDiv(x, y) -> (Ans(FDiv(find x Type.Float regenv, find y Type.Float regenv)), regenv, graph)
      | FDivN(x, y) -> (Ans(FDivN(find x Type.Float regenv, find y Type.Float regenv)), regenv, graph)
      | FInv(x) -> (Ans(FInv(find x Type.Float regenv)), regenv, graph)
      | FSqrt(x) -> (Ans(FSqrt(find x Type.Float regenv)), regenv, graph)

      | Outputb(x) -> (Ans(Outputb(find x Type.Int regenv)), regenv, graph)

      | LdI(x, y) -> (Ans(LdI(find x Type.Int regenv, y)), regenv, graph)
      | StI(x, y, z) -> (Ans(StI(find x Type.Int regenv, find y Type.Int regenv, z)), regenv, graph)
      | LdR(x, y) -> (Ans(LdR(find x Type.Int regenv, find y Type.Int regenv)), regenv, graph)
      | FLdI(x, y) -> (Ans(FLdI(find x Type.Int regenv, y)), regenv, graph)
      | FStI(x, y, z) -> (Ans(FStI(find x Type.Float regenv, find y Type.Int regenv, z)), regenv, graph)
      | FLdR(x, y) -> (Ans(FLdR(find x Type.Int regenv, find y Type.Int regenv)), regenv, graph)

      | IfEq(x, y, e1, e2) -> g'_if dest cont regenv graph ifprefer (fun e1' e2' -> IfEq(find x Type.Int regenv, find y Type.Int regenv, e1', e2')) e1 e2
      | IfLT(x, y, e1, e2) -> g'_if dest cont regenv graph ifprefer (fun e1' e2' -> IfLT(find x Type.Int regenv, find y Type.Int regenv, e1', e2')) e1 e2
      | IfLE(x, y, e1, e2) -> g'_if dest cont regenv graph ifprefer (fun e1' e2' -> IfLE(find x Type.Int regenv, find y Type.Int regenv, e1', e2')) e1 e2
      | IfFEq(x, y, e1, e2) -> g'_if dest cont regenv graph ifprefer (fun e1' e2' -> IfFEq(find x Type.Float regenv, find y Type.Float regenv, e1', e2')) e1 e2
      | IfFLT(x, y, e1, e2) -> g'_if dest cont regenv graph ifprefer (fun e1' e2' -> IfFLT(find x Type.Float regenv, find y Type.Float regenv, e1', e2')) e1 e2
      | IfFLE(x, y, e1, e2) -> g'_if dest cont regenv graph ifprefer (fun e1' e2' -> IfFLE(find x Type.Float regenv, find y Type.Float regenv, e1', e2')) e1 e2

      | CallCls(l, x, ys, zs) ->
	  g'_call dest cont regenv graph (fun ys zs -> CallCls(l, find x Type.Int regenv, ys, zs)) ys zs
      | CallDir(Id.L l, ys, zs) ->
	  g'_call dest cont regenv graph (fun ys zs -> CallDir(Id.L l, ys, zs)) ys zs
      | _ -> assert false

    and g'_if dest cont regenv graph ifprefer constr e1 e2 = (* ifのレジスタ割り当て *)
      let ((e1', regenv1, graph1), (e2', regenv2, graph2)) =
	if ecall e2 then
	  let (e1', regenv1, graph1) = g dest cont regenv graph ifprefer e1 in
	  ((e1', regenv1, graph1), g dest cont regenv graph (M.fold (fun x y p -> addp x [y] p) regenv1 ifprefer) e2)
	else if ecall e1 then
	  let (e2', regenv2, graph2) = g dest cont regenv graph ifprefer e2 in
	  (g dest cont regenv graph (M.fold (fun x y p -> addp x [y] p) regenv2 ifprefer) e1, (e2', regenv2, graph2))
	else (g dest cont regenv graph ifprefer e1, g dest cont regenv graph ifprefer e2) in
      (* aがtrueなら1が,falseなら2が基準 *)
      let a = M.cardinal regenv1 < M.cardinal regenv2 in
      let graphA = if a then graph1 else graph2 in
      let regenvA = if a then regenv1 else regenv2 in
      let regenvB = if a then regenv2 else regenv1 in
      (* AとB両方にある(場所は問わない)変数だけ使う。A基準。 *)
      let (drA, regenvA) =
	match alloc cont regenvA graphA (fst dest) (snd dest) with
	| Spill(y) -> (M.find y regenvA, M.remove y regenvA)
	| Alloc(r) -> (r, regenvA) in
      let (drB, regenvB) =
	match alloc cont regenvB (M.singleton (fst dest) (Colored drA)) (fst dest) (snd dest) with
	| Spill(y) -> (M.find y regenvB, M.remove y regenvB)
	| Alloc(r) -> (r, regenvB) in
      let (rm0, fm0) = 
	match snd dest with
	| Type.Unit -> ([], [])
	| Type.Float -> ([], [(drB, drA)])
	| _ -> ([(drB, drA)], []) in
      let add' a b = if List.mem a b then b else a::b in
      let (regenv', rm, fm, rs) = 
	List.fold_left
	  (fun (regenv', rm, fm, rs) x ->
            try
	      let rA = M.find x regenvA in
	      let rB = M.find x regenvB in
	      let rs' = S.add rA (S.add rB rs) in
	      if rA.[1] = 'r' then
		if List.for_all (fun x -> S.mem x rs') allregs then
		  (regenv', rm, fm, S.add x rs)
		else (add x rA regenv', add' (rB,rA) rm, fm, rs')
	      else if List.for_all (fun x -> S.mem x rs') allfregs then
		(regenv', rm, fm, S.add x rs)
	      else (add x rA regenv', rm, add' (rB,rA) fm, rs')
	    with Not_found -> (regenv', rm, fm, rs))
	  (M.empty, rm0, fm0, S.of_list [drA;drB])
	  (List.filter (fun x -> M.mem x graphA) (fv cont)) in

      (* スワップに使うレジスタを探す *)
      let reg_sw' = 
	List.find (fun x -> not (S.mem x rs)) allregs in
      let reg_fsw' =
	List.find (fun x -> not (S.mem x rs)) allfregs in
      let rm' = List.rev (Emit.shuffle reg_sw' rm) in
      let fm' = List.rev (Emit.shuffle reg_fsw' fm) in
      let movA = match snd dest with
      | Type.Unit -> Nop
      | Type.Float -> FMov(drA)
      | _ -> AddI(drA, 0) in
      let move =
	List.fold_left
	  (fun e (p, fr) -> Let((fr,Type.Float), FMov(p), e))
	  (List.fold_left
	     (fun e (p, r) -> Let((r,Type.Int), AddI(p, 0), e))
	     (Ans(movA))
	     rm')
	  fm' in
      let e1'' = if a || move = Ans(movA) then e1' else concat e1' (drB, snd dest) move in
      let e2'' = if not a || move = Ans(movA) then e2' else concat e2' (drB, snd dest) move in
      (List.fold_left
	 (fun e x ->
	   if x = fst dest || not (M.mem x regenv) || M.mem x regenv' then e else
	   seq(Save(M.find x regenv, x), e)) (* そうでない変数は分岐直前にセーブ *)
	 (Ans(constr e1'' e2''))
	 (fv cont),
       regenv', M.add (fst dest) (Colored drA) graphA)
	

(* 関数呼び出しのレジスタ割り当て *)
    and g'_call dest cont regenv graph constr ys zs = 
      (List.fold_left
	 (fun e x ->
	   if x = fst dest || not (M.mem x regenv) then e else
	   seq(Save(M.find x regenv, x), e))
	 (Ans(constr
		(List.map (fun y -> find y Type.Int regenv) ys)
		(List.map (fun z -> find z Type.Float regenv) zs)))
	 (fv cont),
       M.empty, graph)

let h { name = Id.L(x); args = ys; fargs = zs; body = e; ret = t } = (* 関数のレジスタ割り当て *)
  let regenv = add x reg_cl M.empty in
  let (_, arg_regs, regenv) =
    List.fold_left
      (fun (i, arg_regs, regenv) y ->
        let r = regs.(i) in
        (i + 1,
	 arg_regs @ [r],
	 (assert (not (is_reg y));
	  add y r regenv)))
      (0, [], regenv)
      ys in
  let (_, farg_regs, regenv) =
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
  let (e', _, _) = gc (a, t) (Ans(b a)) regenv M.empty e in
  { name = Id.L(x); args = arg_regs; fargs = farg_regs; body = e'; ret = t }

let f (Prog(fundefs, e)) = (* プログラム全体のレジスタ割り当て *)
  Format.eprintf "register allocation: may take some time%!";
  let fundefs' = List.map h fundefs in
  let (e', _, _) =
    gc (Id.gentmp Type.Unit, Type.Unit) (Ans(Nop)) M.empty M.empty e in
  Format.eprintf "@.";
  Prog(fundefs', e')
