(*pp deriving *)

(* translation into SPARC assembly with infinite number of virtual registers *)

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

type tuple_environment = { tail: bool; top_tuple: (Id.t * int) option }
type tuple_tree = { tuple_name: Id.t; offset: int; store: Asm.t; tuple_type: Type.t }

let generate_tuple_tree env xs =
  let y = Id.genid "t" in
  let (offset, store) =
    expand
      (List.map (fun x -> (x, M.find x env)) xs)
      (1, Ans(AddI(y, 0)))
      (fun x offset store ->  seq(FStI(x, y, offset), store))
      (fun x _ offset store -> seq(StI(x, y, offset), store)) in
  { tuple_name = y; offset = offset; store = store; tuple_type = Type.Tuple(List.map (fun x -> M.find x env) xs) }

(* 式の仮想マシンコード生成 *)
let rec g tuple_env env e =
  let down = g tuple_env env in
  match e with
    | Closure.Unit   -> Ans(Nop)
    | Closure.Int(i) -> Ans(Int(i))
    | Closure.Float(d) -> Ans(Float(d))
    | Closure.Neg(x) -> Ans(MulI(x, -1))
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
        | Type.Bool | Type.Int -> Ans(IfEq(x, y, down e1, down e2))
        | Type.Float -> Ans(IfFEq(x, y, down e1, down e2))
        | typ -> failwith (
          Printf.sprintf"equality supported only for bool, int, and float, but is %s\n%s"
            (Type.show typ)
            (Show.show<Closure.t> e)))
    | Closure.IfLE(x, y, e1, e2) ->
      (match M.find x env with
        | Type.Bool | Type.Int -> Ans(IfLE(x, y, down e1, down e2))
        | Type.Float -> Ans(IfFLE(x, y, down e1, down e2))
        | _ -> failwith "inequality supported only for bool, int, and float")
    | Closure.IfLT(x, y, e1, e2) ->
      (match M.find x env with
        | Type.Bool | Type.Int -> Ans(IfLT(x, y, down e1, down e2))
        | Type.Float -> Ans(IfFLT(x, y, down e1, down e2))
        | _ -> failwith "inequality supported only for bool, int, and float")
    | Closure.IfNil(x, e1, e2) ->
      (match M.find x env with
        | Type.List(_) ->
            (* take the cdr and check that is 0 *)
          let cdr = Id.genid "i" in
          Let((cdr, Type.Int), LdI(x, 1), Ans(IfEq(cdr, reg_0, down e1, down e2)))
        | _ -> failwith "the argument of IfNil is not a list")
    | Closure.Let((x, (Type.Tuple(types) as t1)), e1, e2) ->
      let { top_tuple = top_tuple; tail = tail } = tuple_env in
      let e1' = g { tail = false; top_tuple = top_tuple } env e1 in
      let e2' = g { tail = tail; top_tuple = (Some(x, List.length types + 1)) } (M.add x t1 env) e2 in
      concat e1' (x, t1) e2'
    | Closure.Let((x, t1), e1, e2) ->
      let { top_tuple = top_tuple; _ } = tuple_env in
      let e1' = g { tail = false; top_tuple = top_tuple } env e1 in
      let e2' = g tuple_env (M.add x t1 env) e2 in
      concat e1' (x, t1) e2'
    | Closure.Var(x) ->
      (match M.find x env with
        | Type.Unit -> Ans(Nop)
        | Type.Float -> Ans(FMov(x))
        | _ -> Ans(AddI(x, 0)))
    | Closure.MakeCls((x, t), { Closure.entry = l; Closure.actual_fv = ys }, e2) -> (* クロージャの生成 *)
        (* Closureのアドレスをセットしてから、自由変数の値をストア *)
      let e2' = g tuple_env (M.add x t env) e2 in
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
      let (int, float) = separate (List.map (fun y -> (y, M.find y env)) ys) in
      Ans(CallDir(Id.L(x), int, float))
    | Closure.Tuple(xs) -> (* 組の生成 *)
      let {tuple_name = y; tuple_type = tuple_type; offset = offset; store = store} = generate_tuple_tree env xs in
      (match tuple_env with
        | {tail = true; _} ->
          Let((y, tuple_type), AddI(reg_fp, 0), store)
        | _ ->  (* It is still possible in recursion *)
          Let((y, Type.Tuple(List.map (fun x -> M.find x env) xs)), AddI(reg_hp, 0),
              Let((reg_hp, Type.Int), AddI(reg_hp, offset),
                  store)))
    | Closure.LetTuple(xts, y, e2) ->
      let s = Closure.fv e2 in
      let inner_exp =
        match tuple_env with
          | { top_tuple = Some((name, offset)); _} when name = y && not (S.mem name (Closure.fv e2)) ->
            (1, Let((reg_hp, Type.Int), SubI(reg_hp, offset), g tuple_env (M.add_list xts env) e2))
          | _ ->
            (1, g tuple_env (M.add_list xts env) e2)
      in
      let (_, load) =
        expand
          xts
          inner_exp
          (fun x offset load ->
            if not (S.mem x s) then load else (* [XX] a little ad hoc optimization *)
              fletd(x, FLdI(y, offset), load))
          (fun x t offset load ->
            if not (S.mem x s) then load else (* [XX] a little ad hoc optimization *)
              Let((x, t), LdI(y, offset), load)) in
      load
    | Closure.Get(x, y) -> (* 配列の読み出し *)
      (match M.find x env with
        | Type.Array(Type.Unit) -> Ans(Nop)
        | Type.Array(Type.Float) ->
          Ans(FLdR(x, y))
        | Type.Array(_) ->
          Ans(LdR(x, y))
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
        g tuple_env (add_list_matcher_with_direct_type matcher typ env) rest
      in
      (
        match matcher with
          | Syntax.ListWithNil(vars) ->
            expand_let_list (vars @ [Id.genid "dummy_list"]) typ list_id expanded_rest
          | Syntax.ListWithoutNil(vars) ->
            expand_let_list vars typ list_id expanded_rest
      )

(* トップレベルの関数の仮想マシンコード生成 *)
let h  { Closure.name = (Id.L(x), t); Closure.args = yts; Closure.formal_fv = zts; Closure.body = e } =
  let (int, float) = separate yts in
  let b = if List.mem x !Closure.danger then false else true in
  let (_, load) =
    expand
      zts
      (1, g { tail = b; top_tuple = None } (M.add x t (M.add_list yts (M.add_list zts M.empty))) e)
      (fun z offset load -> fletd(z, FLdI(reg_cl, offset), load))
      (fun z t offset load -> Let((z, t), LdI(reg_cl, offset), load)) in
  match t with
    | Type.Fun(_, t2) ->
      { name = Id.L(x); args = int; fargs = float; body = load; ret = t2 }
    | _ -> assert false

(* プログラム全体の仮想マシンコード生成 *)
let f (Closure.Prog(fundefs, e)) =
  Closure.danger := [];
  let fundefs = List.map h fundefs in
  let e = g { tail = false; top_tuple = None } M.empty e in
  Prog (fundefs, e)
