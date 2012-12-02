(* translation into assembly with infinite number of virtual registers *)

open Asm

(* グローバル配列の集合 *)
let globals = ref M.empty


(* その数が2の何乗か返す(切り上げ) *)
let rec log2_sub n i =
  if 1 lsl i >= n then i
  else log2_sub n (i+1)
let log2 n = log2_sub n 0

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


let expand_let_list (variables:Id.t list) typ root_list_var rest =
  let constructor = match typ with
    | Type.Float -> (fun var offset -> FLdI(var, offset))
    | _          -> (fun var offset -> LdI(var, offset))
  in
  let list_typ = Type.List(ref (Some typ)) in
  let rec iter list_var = function
    | []  -> assert false
    | [_] -> assert false
    | v :: last :: [] ->
      Let((v, typ), constructor list_var 0,
          Let((last, list_typ), LdI(list_var, 1), rest))
    | v :: vs ->
      let temp_list_var = Id.genid "temp_list" in
      Let((v, typ), constructor list_var 0,
          Let((temp_list_var, list_typ), LdI(list_var, 1), iter temp_list_var vs))
  in
  iter root_list_var variables

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
let rec g tail env = function 
  | Closure.Let((x, t1), exp, e) ->
      let exp' = g' false env exp in
      let e' = g tail (M.add x t1 env) e in
      concat exp' (x, t1) e'
  | Closure.MakeCls((x, t), { Closure.entry = l; Closure.actual_fv = ys }, e2) -> (* クロージャの生成 *)
      (* Closureのアドレスをセットしてから、自由変数の値をストア *)
      let e2' = g tail (M.add x t env) e2 in
      let offset, store_fv =
	expand
	  (List.map (fun y -> (y, M.find y env)) ys)
	  (1, e2')
	  (fun y offset store_fv ->  seq(FStI(y, x, offset), store_fv))
	  (fun y _ offset store_fv -> seq(StI(y, x, offset), store_fv)) in
      let z = Id.genid "l" in
      (match t with
      | Type.Fun(_,_) ->
	  if M.mem x !globals then
	    Let((x, t), Int(M.find x !globals),
		Let((z, Type.Int), SetL(l),
		    seq(StI(z, x, 0),
			store_fv)))
	  else
	    Let((x, t), AddI(reg_hp, 0),
	        Let((reg_hp, Type.Int), AddI(reg_hp, offset),
	  	    Let((z, Type.Int), SetL(l),
		        seq(StI(z, x, 0),
		  	    store_fv))))
      | _ -> assert false)
  | Closure.LetTuple(xts, y, e2) ->
      let (_, load) =
	expand
	  xts
	  (0, g tail (M.add_list xts env) e2)
	  (fun x offset load ->
	    insert_load x (fun a -> Let((x, Type.Float), FLdI(y, offset), a)) load)
	  (fun x t offset load ->
	    insert_load x (fun a -> Let((x, t), LdI(y, offset), a)) load) in
      load
  | Closure.LetList((matcher, typ), list_id, rest) ->
      (* Here, a variable should not have Var type, but the content of it (s.t. Float) *)
      let add_list_matcher_with_direct_type matcher typ env =
        let add_type_variables matcher typ =
          match matcher with
          | Syntax.ListWithNil(variables) -> List.map (fun v -> (v, typ)) variables
          | Syntax.ListWithoutNil(variables) ->
              let reversed   = List.rev variables in
              let list_var   = List.hd reversed in
              let other_vars = List.tl reversed in
              (list_var, Type.List(ref (Some typ))) :: (List.map (fun v -> (v, typ)) other_vars)
        in
        M.add_list (add_type_variables matcher typ) env
      in
      let expanded_rest =
        g tail (add_list_matcher_with_direct_type matcher typ env) rest
      in
      (
       match matcher with
       | Syntax.ListWithNil(vars) ->
           expand_let_list (vars @ [Id.genid "dummy_list"]) typ list_id expanded_rest
       | Syntax.ListWithoutNil(vars) ->
           expand_let_list vars typ list_id expanded_rest
      )
  | Closure.Ans(exp) -> g' tail env exp
and g' tail env = function
  | Closure.Unit   -> Ans(Nop)
  | Closure.Int(i) -> Ans(Int(i))
  | Closure.Float(d) -> Ans(Float(d))
  | Closure.Neg(x) -> Ans(Sub(reg_0, x))
  | Closure.Add(x, y) -> Ans(Add(x, y))
  | Closure.Sub(x, y) -> Ans(Sub(x, y))
  | Closure.Mul(x, y) -> Ans(CallDir(Id.L("min_caml_mul"),[x;y],[]))
  | Closure.Sll(x, y) -> Ans(SllI(x, y))
  | Closure.Sra(x, y) -> Ans(SraI(x, y))
  | Closure.FNeg(x) -> Ans(FNeg(x))
  | Closure.FAdd(x, y) -> Ans(FAdd(x, y))
  | Closure.FSub(x, y) -> Ans(FSub(x, y))
  | Closure.FMul(x, y) -> Ans(FMul(x, y))
  | Closure.FDiv(x, y) -> Ans(FDiv(x, y))
  | Closure.IfEq(x, y, e1, e2) ->
      (match M.find x env with
      | Type.Bool | Type.Int -> Ans(IfEq(x, y, g tail env e1, g tail env e2))
      | Type.Float -> Ans(IfFEq(x, y, g tail env e1, g tail env e2))
      | _ -> failwith "equality supported only for bool, int, and float")
  | Closure.IfLE(x, y, e1, e2) ->
      (match M.find x env with
      | Type.Bool | Type.Int -> Ans(IfLE(x, y, g tail env e1, g tail env e2))
      | Type.Float -> Ans(IfFLE(x, y, g tail env e1, g tail env e2))
      | _ -> failwith "inequality supported only for bool, int, and float")
  | Closure.IfLT(x, y, e1, e2) ->
      (match M.find x env with
      | Type.Bool | Type.Int -> Ans(IfLT(x, y, g tail env e1, g tail env e2))
      | Type.Float -> Ans(IfFLT(x, y, g tail env e1, g tail env e2))
      | _ -> failwith "inequality supported only for bool, int, and float")
  | Closure.IfNil(x, e1, e2) ->
      (match M.find x env with
      | Type.List(_) ->
          (* take the cdr and check that is 0 *)
          let cdr = Id.genid "i" in
          Let((cdr, Type.Int), LdI(x, 1), Ans(IfEq(cdr, reg_0, g tail env e1, g tail env e2)))
      | _ -> failwith "the argument of IfNil is not a list")
  | Closure.Var(x) ->
      (match M.find x env with
      | Type.Unit -> Ans(Nop)
      | Type.Float -> Ans(FMov(x))
      | _ -> Ans(AddI(x, 0)))
  | Closure.AppCls(x, ys) ->
      let (int, float) = separate (List.map (fun y -> (y, M.find y env)) ys) in
      Ans(CallCls(Id.L(x), x, int, float))
  | Closure.AppDir(Id.L(x), ays) ->
      let ayts = List.map (fun y -> (y, M.find y env)) ays in
      let (int, float) = separate ayts in
      (* xor等を変換 *)
      (match x with
      | "min_caml_xor" -> Ans(Xor(List.nth int 0, List.nth int 1))
      | "min_caml_sqrt" -> Ans(FSqrt(List.hd float))
      | "min_caml_not" -> Ans(Xor(List.hd int, reg_1))
      | "min_caml_print_char" -> Ans(Outputb(List.hd int))
      | "min_caml_input_char" | "min_caml_read_char" -> Ans(Inputb)
      | "min_caml_create_tuple_array" ->
	  let z = List.hd ays in
	  let yts = List.tl ayts in
	  let r = Id.genid "t" in
	  let t = Id.genid "T" in
	  let l = List.length yts in
	  let ll = log2 l in
	  let l' = 1 lsl ll in
	  let (_, store) =
	    expand
	      yts
	      (0, Ans(AddI(r, 0)))
	      (fun y offset store ->
		let off = Id.genid "Off" in
		let n = Id.genid "n" in
	      	Let((off, Type.Int), AddI(r, offset),
		    Let((n, Type.Int), Int(l'),
			seq(CallDir(Id.L("min_caml_float_tuple_array"), [off; n], [y]), store))))
	      (fun y _ offset store ->
		let off = Id.genid "Off" in
		let n = Id.genid "n" in
	      	Let((off, Type.Int), AddI(r, offset),
		    Let((n, Type.Int), Int(l'),
			seq(CallDir(Id.L("min_caml_int_tuple_array"), [y; off; n], []), store)))) in
	  Let((r, Type.Array(Type.Tuple(List.map snd yts))), AddI(reg_hp, 0),
	      Let((t, Type.Int), SllI(z, ll),
		  (Let((reg_hp, Type.Int), Add(reg_hp, t),
		  	   store))))
      | "min_caml_tuple_array_init" ->
	  let r = List.hd ays in
	  let z = List.nth ays 1 in
	  let yts = List.tl (List.tl ayts) in
	  let t = Id.genid "T" in
	  let l = List.length yts in
	  let ll = log2 l in
	  let l' = 1 lsl ll in
	  let addr = Id.genid "addr" in
	  let (_, store) =
	    expand
	      yts
	      (0, Ans(Nop))
	      (fun y offset store ->
		let off = Id.genid "Off" in
		let n = Id.genid "n" in
	      	Let((off, Type.Int), AddI(r, offset),
		    Let((n, Type.Int), Int(l'),
			seq(CallDir(Id.L("min_caml_float_tuple_array_init"), [off; n; addr], [y]), store))))
	      (fun y _ offset store ->
		let off = Id.genid "Off" in
		let n = Id.genid "n" in
	      	Let((off, Type.Int), AddI(r, offset),
		    Let((n, Type.Int), Int(l'),
			seq(CallDir(Id.L("min_caml_int_tuple_array_init"), [y; off; n; addr], []), store)))) in
	  Let((t, Type.Int), SllI(z, ll),
	      (Let((addr, Type.Int), Add(r, t),
		   store)))
      | _ -> Ans(CallDir(Id.L(x), int, float)))
  | Closure.Tuple(xs) -> (* 組の生成 *)
      let y = Id.genid "t" in
      let (offset, store) =
	expand
	  (List.map (fun x -> (x, M.find x env)) xs)
	  (0, Ans(AddI(y, 0)))
	  (fun x offset store ->  seq(FStI(x, y, offset), store))
	  (fun x _ offset store -> seq(StI(x, y, offset), store)) in
      if tail then
	Let((y, Type.Tuple(List.map (fun x -> M.find x env) xs)), AddI(reg_fp, 1), store)
      else
	Let((y, Type.Tuple(List.map (fun x -> M.find x env) xs)), AddI(reg_hp, 0),
	    Let((reg_hp, Type.Int), AddI(reg_hp, offset),
		store))
  | Closure.Get(x, y) -> (* 配列の読み出し *)
      (match M.find x env with
      | Type.Array(Type.Unit) -> Ans(Nop)
      | Type.Array(Type.Float) -> Ans(FLdR(x, y))
      | Type.Array _ -> Ans(LdR(x, y))
      | _ -> assert false)
  | Closure.Put(x, y, z) ->
      let offset = Id.genid "o" in
      (match M.find x env with
      | Type.Array(Type.Unit) -> Ans(Nop)
      | Type.Array(Type.Float) ->
	  Let((offset, Type.Int), Add(x, y),
	      Ans(FStI(z, offset, 0)))
      | Type.Array _ ->
	  Let((offset, Type.Int), Add(x, y),
	      Ans(StI(z, offset, 0)))
      | _ -> assert false)
	(* タプルを配列から読み出す時に使う命令 *)
  | Closure.GetTuple(x, y) ->
      (match M.find x env with
      | Type.Array(Type.Tuple(ts)) ->
          let ind = Id.genid "Index" in
	  let l = List.length ts in
	  Let((ind, Type.Int), SllI(y, log2 l), (Ans(Add(x, ind))))
      | _ -> assert false)
	(* タプルを配列に埋め込む時に使う命令 *)
  | Closure.PutTuple(x, y, zs) ->
      (match M.find x env with
      | Type.Array(Type.Tuple(ts)) ->
          let ind = Id.genid "Index" in
          let addr = Id.genid "Addr" in
	  let l = List.length ts in
          let (_, store) =
	    expand
	      (List.map (fun x -> (x, M.find x env)) zs)
	      (0, Ans(Nop))
	      (fun w offset store ->  seq(FStI(w, addr, offset), store))
	      (fun w _ offset store -> seq(StI(w, addr, offset), store)) in
	  Let((ind, Type.Int), SllI(y, log2 l),
	      (Let((addr, Type.Tuple(ts)), Add(x, ind), store)))
      | _ -> assert false)
  | Closure.ExtArray(Id.L(x)) when M.mem x !globals
    -> Ans(Int(M.find x !globals))
  | Closure.ExtArray(Id.L(x)) -> Ans(SetL(Id.L("min_caml_" ^ x)))
  | Closure.Nil ->
      let new_list = Id.genid "li" in
      Let((new_list, Type.List(ref (None))), AddI(reg_hp, 0),
          Let((reg_hp, Type.Int), AddI(reg_hp, 2),
              seq (StI(reg_0, new_list, 0), seq (StI(reg_0, new_list, 1), Ans(AddI(new_list, 0))))))
  | Closure.Cons(x, y) ->
      let element_type = M.find x env in
      let typed_store = (function
        | Type.Float -> (fun x y offset -> FStI(x, y, offset))
        | _ -> (fun x y offset -> StI(x, y, offset))) element_type
      in
      let new_list = Id.genid "li" in
      Let((new_list, Type.List(ref (Some element_type))), AddI(reg_hp, 0),
          Let((reg_hp, Type.Int), AddI(reg_hp, 2),
              seq (typed_store x new_list 0, seq (StI(y, new_list, 1), Ans(AddI(new_list, 0))))))


(* トップレベルの関数の仮想マシンコード生成 *)
let h { Closure.name = (Id.L(x), t); Closure.args = yts; Closure.formal_fv = zts; Closure.body = e } =
  let (int, float) = separate yts in
  let tail = if S.mem x !CollectDanger.danger then false else true in
  let (x', head) =
    if M.mem x !globals then
      let x' = Id.genid x in
      (x', (fun p -> (Let((x', t), Int(M.find x !globals), p))))
    else (x, (fun t -> t)) in
  let (_, load) =
    expand
      zts
      (1, g tail (M.add x t (M.add_list yts (M.add_list zts M.empty))) e)
      (fun z offset load ->
	insert_load z (fun a -> Let((z, Type.Float), FLdI(x', offset), a)) load)
      (fun z t offset load ->
	insert_load z (fun a -> Let((z, t), LdI(x', offset), a)) load) in
  match t with
  | Type.Fun(_, t2) ->
      { name = Id.L(x); args = int; fargs = float; body = head load; ret = t2 }
  | _ -> assert false


(* グローバル配列の仮想マシンコード生成 *)
let i { Closure.gname = (Id.L(x), _); Closure.length = l } =
  let hp_back = !hp in
  hp := !hp + l;
  globals := M.add x hp_back !globals


(* プログラム全体の仮想マシンコード生成 *)
let f (Closure.Prog(gls, fundefs, e)) =
  Format.eprintf "generating virtual assembly...@.";
  List.iter i gls;
  let fundefs = List.map h fundefs in
  let e = g false M.empty e in
  Prog (fundefs, e)
