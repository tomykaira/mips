(* translation into assembly with infinite number of virtual registers *)

open Asm

(* 変数と型の組をintとfloatに分類する関数 *)
let classify xts ini addf addi =
  List.fold_left
    (fun acc (x, t) ->
      match t with
      | Type.Unit -> acc
      | Type.Float -> addf acc x
      | _ -> addi acc x t)
    ini
    xts

(* 変数と型の組のリストをintとfloatに分ける関数 *)
let separate xts =
  classify
    xts
    ([], [])
    (fun (int, float) x -> (int, float @ [x]))
    (fun (int, float) x _ -> (int @ [x], float))

let expand xts ini addf addi =
  classify
    xts
    ini
    (fun (offset, acc) x ->
      (offset + 1, addf x offset acc))
    (fun (offset, acc) x t ->
      (offset + 1, addi x t offset acc))

(* ロード命令を,その変数を使う直前に挟み込む関数 *)
let rec insert_load x load = function
  | Let(xt, exp, e) as e' ->
      if List.mem x (fv_exp exp) then
	if List.mem x (fv e) then load e'
	else concat (insert_load' x load exp) xt e
      else Let(xt, exp, insert_load x load e)
  | Ans(exp) ->
      if List.mem x (fv_exp exp) then insert_load' x load exp
      else Ans(exp)
and insert_load' x load = function
  | IfEq(y,z,e1,e2) as exp ->
      if x = y || x = z then load (Ans(exp))
      else Ans(IfEq(y, z, insert_load x load e1, insert_load x load e2))
  | IfLE(y,z,e1,e2) as exp ->
      if x = y || x = z then load (Ans(exp))
      else Ans(IfLE(y, z, insert_load x load e1, insert_load x load e2))
  | IfLT(y,z,e1,e2) as exp ->
      if x = y || x = z then load (Ans(exp))
      else Ans(IfLT(y, z, insert_load x load e1, insert_load x load e2))
  | IfFEq(y,z,e1,e2) as exp ->
      if x = y || x = z then load (Ans(exp))
      else Ans(IfFEq(y, z, insert_load x load e1, insert_load x load e2))
  | IfFLE(y,z,e1,e2) as exp ->
      if x = y || x = z then load (Ans(exp))
      else Ans(IfFLE(y, z, insert_load x load e1, insert_load x load e2))
  | IfFLT(y,z,e1,e2) as exp -> 
      if x = y || x = z then load (Ans(exp))
      else Ans(IfFLT(y, z, insert_load x load e1, insert_load x load e2))
  | exp -> load (Ans(exp))

(* 式の仮想マシンコード生成 *)
let rec g env = function 
  | Closure.Unit   -> Ans(Nop)
  | Closure.Int(i) -> Ans(Int(i))
  | Closure.Float(d) -> Ans(Float(d))
  | Closure.Neg(x) -> Ans(Sub(reg_0, x))
  | Closure.Add(x, y) -> Ans(Add(x, y))
  | Closure.Sub(x, y) -> Ans(Sub(x, y))
  | Closure.Mul(x, y) -> Ans(Mul(x, y))
  | Closure.Sll(x, y) -> Ans(SllI(x, y))
  | Closure.Sra(x, y) -> Ans(SraI(x, y))
  | Closure.FNeg(x) -> Ans(FNeg(x))
  | Closure.FAdd(x, y) -> Ans(FAdd(x, y))
  | Closure.FSub(x, y) -> Ans(FSub(x, y))
  | Closure.FMul(x, y) -> Ans(FMul(x, y))
  | Closure.FDiv(x, y) -> Ans(FDiv(x, y))
  | Closure.IfEq(x, y, e1, e2) ->
      (match M.find x env with
      | Type.Bool | Type.Int -> Ans(IfEq(x, y, g env e1, g env e2))
      | Type.Float -> Ans(IfFEq(x, y, g env e1, g env e2))
      | _ -> failwith "equality supported only for bool, int, and float")
  | Closure.IfLE(x, y, e1, e2) ->
      (match M.find x env with
      | Type.Bool | Type.Int -> Ans(IfLE(x, y, g env e1, g env e2))
      | Type.Float -> Ans(IfFLE(x, y, g env e1, g env e2))
      | _ -> failwith "inequality supported only for bool, int, and float")
 | Closure.IfLT(x, y, e1, e2) ->
      (match M.find x env with
      | Type.Bool | Type.Int -> Ans(IfLT(x, y, g env e1, g env e2))
      | Type.Float -> Ans(IfFLT(x, y, g env e1, g env e2))
      | _ -> failwith "inequality supported only for bool, int, and float")
  | Closure.Let((x, t1), e1, e2) ->
      let e1' = g env e1 in
      let e2' = g (M.add x t1 env) e2 in
      concat e1' (x, t1) e2'
  | Closure.Var(x) ->
      (match M.find x env with
      | Type.Unit -> Ans(Nop)
      | Type.Float -> Ans(FMov(x))
      | _ -> Ans(AddI(x, 0)))
  | Closure.MakeCls((x, t), { Closure.entry = l; Closure.actual_fv = ys }, e2) -> (* クロージャの生成 *)
      (* Closureのアドレスをセットしてから、自由変数の値をストア *)
      let e2' = g (M.add x t env) e2 in
      let offset, store_fv =
	expand
	  (List.map (fun y -> (y, M.find y env)) ys)
	  (1, e2')
	  (fun y offset store_fv ->  seq(FStI(y, x, offset), store_fv))
	  (fun y _ offset store_fv -> seq(StI(y, x, offset), store_fv)) in
      Let((x, t), AddI(reg_hp, 0),
	  Let((reg_hp, Type.Int), AddI(reg_hp, offset),
	      let z = Id.genid "l" in
	      Let((z, Type.Int), SetL(l),
		  seq(StI(z, x, 0),
		      store_fv))))
  | Closure.AppCls(x, ys) ->
      let (int, float) = separate (List.map (fun y -> (y, M.find y env)) ys) in
      Ans(CallCls(x, int, float))
  | Closure.AppDir(Id.L(x), ys) ->
      let ayts = List.map (fun y -> (y, M.find y env)) ys in
      let (int, float) = separate ayts in
      (* xor等を変換 *)
      (match x with
      | "min_caml_xor" -> Ans(Xor(List.nth int 0, List.nth int 1))
      | "min_caml_sqrt" -> Ans(FSqrt(List.hd float))
      | "min_caml_not" -> Ans(Xor(List.hd int, reg_1))
      | "min_caml_create_tuple_array" ->
	  let z = fst (List.hd ayts) in
	  let yts = List.tl ayts in
	  let r = Id.genid "t" in
	  let n = Id.genid "n" in
	  let t = Id.genid "T" in
	  let l = List.length yts in
	  let (_, store) =
	    expand
	      yts
	      (0, Ans(AddI(r, 0)))
	      (fun y offset store ->
		let off = Id.genid "Off" in
	      	Let((off, Type.Int), AddI(r, offset),
		    seq(CallDir(Id.L("min_caml_float_tuple_array"), [off; n], [y]), store)))
	      (fun y _ offset store ->
		let off = Id.genid "Off" in
		Let((off, Type.Int), AddI(r, offset),
		    seq(CallDir(Id.L("min_caml_int_tuple_array"), [y; off; n], []), store))) in
	  Let((r, Type.Array(Type.Tuple(List.map snd yts))), AddI(reg_hp, 0),
	      Let((n, Type.Int), Int(l),
		  Let((t, Type.Int), Mul(n, z),
		      Let((reg_hp, Type.Int), Add(reg_hp, t),
			  store))))
      | _ -> Ans(CallDir(Id.L(x), int, float)))
  | Closure.Tuple(xs) -> (* 組の生成 *)
      let y = Id.genid "t" in
      let (offset, store) =
	expand
	  (List.map (fun x -> (x, M.find x env)) xs)
	  (0, Ans(AddI(y, 0)))
	  (fun x offset store ->  seq(FStI(x, y, offset), store))
	  (fun x _ offset store -> seq(StI(x, y, offset), store)) in
      Let((y, Type.Tuple(List.map (fun x -> M.find x env) xs)), AddI(reg_hp, 0),
	  Let((reg_hp, Type.Int), AddI(reg_hp, offset),
	      store))
  | Closure.LetTuple(xts, y, e2) ->
      let (_, load) =
	expand
	  xts
	  (0, g (M.add_list xts env) e2)
	  (fun x offset load ->
	    insert_load x (fun a -> Let((x, Type.Float), FLdI(y, offset), a)) load)
	  (fun x t offset load ->
	    insert_load x (fun a -> Let((x, t), LdI(y, offset), a)) load) in
      load
  | Closure.Get(x, y) -> (* 配列の読み出し *)
      (match M.find x env with
      | Type.Array(Type.Unit) -> Ans(Nop)
      | Type.Array(Type.Float) -> Ans(FLdR(x, y))
      | Type.Array(_) -> Ans(LdR(x, y))
      | _ -> assert false)
  | Closure.Put(x, y, z) ->
      let offset = Id.genid "o" in
      (match M.find x env with
      | Type.Array(Type.Unit) -> Ans(Nop)
      | Type.Array(Type.Float) ->
	  Let((offset, Type.Int), Add(x, y),
	      Ans(FStI(z, offset, 0)))
      | Type.Array(_) ->
	  Let((offset, Type.Int), Add(x, y),
	      Ans(StI(z, offset, 0)))
      | _ -> assert false)
  (* タプルを配列に埋め込む時に使う命令 *)
  | Closure.PutTuple(x, y, zs) ->
      (match M.find x env with
      | Type.Array(Type.Tuple(ts))->
	  let len = Id.genid "Len" in
          let ind = Id.genid "Ind" in
          let addr = Id.genid "Add" in
          let (_, store) =
	    expand
	      (List.map (fun x -> (x, M.find x env)) zs)
	      (0, Ans(Nop))
	      (fun w offset store ->  seq(FStI(w, addr, offset), store))
	      (fun w _ offset store -> seq(StI(w, addr, offset), store)) in
	  Let((len, Type.Int), Int(List.length ts),
	      Let((ind, Type.Int), Mul(len, y),
		  Let((addr, Type.Int), Add(x, ind), store)))
      | _ -> assert false)
  | Closure.ExtArray(Id.L(x)) -> Ans(SetL(Id.L("min_caml_" ^ x)))

(* トップレベルの関数の仮想マシンコード生成 *)
let h { Closure.name = (Id.L(x), t); Closure.args = yts; Closure.formal_fv = zts; Closure.body = e } =
  let (int, float) = separate yts in
  let (_, load) =
    expand
      zts
      (1, g (M.add x t (M.add_list yts (M.add_list zts M.empty))) e)
      (fun z offset load ->
	insert_load z (fun a -> Let((z, Type.Float), FLdI(x, offset), a)) load)
      (fun z t offset load ->
	insert_load z (fun a -> Let((z, t), LdI(x, offset), a)) load) in
  match t with
  | Type.Fun(_, t2) ->
      { name = Id.L(x); args = int; fargs = float; body = load; ret = t2 }
  | _ -> assert false

(* プログラム全体の仮想マシンコード生成 *)
let f (Closure.Prog(fundefs, e)) =
  Format.eprintf "generating virtual assembly...@.";
  let fundefs = List.map h fundefs in
  let e = g M.empty e in
  Prog (fundefs, e)
