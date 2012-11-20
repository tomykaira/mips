(*pp deriving *)

(* type inference/reconstruction *)

open Syntax

exception Unify of Type.t * Type.t
exception Error of t * Type.t * Type.t

let extenv = ref M.empty

(* for pretty printing (and type normalization) *)
let rec deref_typ = function (* 型変数を中身でおきかえる関数 *)
  | Type.Fun(t1s, t2) -> Type.Fun(List.map deref_typ t1s, deref_typ t2)
  | Type.Tuple(ts) -> Type.Tuple(List.map deref_typ ts)
  | Type.Array(t) -> Type.Array(deref_typ t)
  | Type.Var({ contents = None } as r) ->
    Format.eprintf "uninstantiated type variable detected; assuming int@.";
    r := Some(Type.Int);
    Type.Int
  | Type.Var({ contents = Some(t) } as r) ->
    let t' = deref_typ t in
    r := Some(t');
    t'
  | Type.List({ contents = None }) ->
    failwith "uninstantiated type variable in List detected@."
  | Type.List({ contents = Some(t) } as r) as list_typ ->
    let t' = deref_typ t in
    r := Some(t');
    list_typ
  | t -> t
let rec deref_id_typ (x, t) = (x, deref_typ t)
let rec deref_term = function
  | Not(e) -> Not(deref_term e)
  | Neg(e) -> Neg(deref_term e)
  | Add(e1, e2) -> Add(deref_term e1, deref_term e2)
  | Sub(e1, e2) -> Sub(deref_term e1, deref_term e2)
  | Mul(e1, e2) -> Mul(deref_term e1, deref_term e2)
  | Sll(e1, e2) -> Sll(deref_term e1, e2)
  | Sra(e1, e2) -> Sra(deref_term e1, e2)
  | Eq(e1, e2) -> Eq(deref_term e1, deref_term e2)
  | LE(e1, e2) -> LE(deref_term e1, deref_term e2)
  | LT(e1, e2) -> LT(deref_term e1, deref_term e2)
  | FNeg(e) -> FNeg(deref_term e)
  | FAdd(e1, e2) -> FAdd(deref_term e1, deref_term e2)
  | FSub(e1, e2) -> FSub(deref_term e1, deref_term e2)
  | FMul(e1, e2) -> FMul(deref_term e1, deref_term e2)
  | FDiv(e1, e2) -> FDiv(deref_term e1, deref_term e2)
  | If(e1, e2, e3) -> If(deref_term e1, deref_term e2, deref_term e3)
  | Let(xt, e1, e2) -> Let(deref_id_typ xt, deref_term e1, deref_term e2)
  | LetRec({ name = xt; args = yts; body = e1 }, e2) ->
    LetRec({ name = deref_id_typ xt;
             args = List.map deref_id_typ yts;
             body = deref_term e1 },
           deref_term e2)
  | App(e, es) -> App(deref_term e, List.map deref_term es)
  | Tuple(es) -> Tuple(List.map deref_term es)
  | LetTuple(xts, e1, e2) -> LetTuple(List.map deref_id_typ xts, deref_term e1, deref_term e2)
  | Array(e1, e2) -> Array(deref_term e1, deref_term e2)
  | Get(e1, e2) -> Get(deref_term e1, deref_term e2)
  | Put(e1, e2, e3) -> Put(deref_term e1, deref_term e2, deref_term e3)
  | Match(e1, cases) -> Match(deref_term e1, List.map (fun (pattern, body) -> (pattern, deref_term body)) cases)
  | IsNil(e) -> IsNil(deref_term e)
  | Cons(e1, e2) -> Cons(deref_term e1, deref_term e2)
  | LetList((matcher, { contents = Some(typ)}), e1, e2) -> LetList((matcher, ref (Some(deref_typ typ))), deref_term e1, deref_term e2)
  | LetList((matcher, { contents = None}), e1, e2)      -> failwith "Type of LetList is expected to be fixed"
  | e -> e

let rec occur r1 = function (* occur check *)
  | Type.Fun(t2s, t2) -> List.exists (occur r1) t2s || occur r1 t2
  | Type.Tuple(t2s) -> List.exists (occur r1) t2s
  | Type.Array(t2) -> occur r1 t2
  | Type.Var(r2) when r1 == r2 -> true
  | Type.Var({ contents = None }) -> false
  | Type.Var({ contents = Some(t2) }) -> occur r1 t2
  | Type.List({ contents = Some(t2) }) -> occur r1 t2
  | _ -> false

let rec part k l ret = match (k, l) with
| [], ys -> (ret, ys)
| _::xs, y::ys -> part xs ys (ret@[y])
| _ -> raise (Invalid_argument "part")
let rec unify t1 t2 = (* 型が合うように、型変数への代入をする *)
  match t1, t2 with
  | Type.Unit, Type.Unit | Type.Bool, Type.Bool | Type.Int, Type.Int | Type.Float, Type.Float -> ()
	(* 関数の引数の数が違ったら,長い方をカリー化して比較 *)
  | Type.Fun(t1s, t1'), Type.Fun(t2s, t2') when (List.length t1s) < (List.length t2s) ->
      let t2s1, t2s2 = part t1s t2s [] in
      unify (Type.Fun(t1s, t1')) (Type.Fun(t2s1, Type.Fun(t2s2, t2')))
  | Type.Fun(t1s, t1'), Type.Fun(t2s, t2') when List.length t1s > List.length t2s ->
      let t1s1, t1s2 = part t2s t1s [] in
      unify (Type.Fun(t1s1, Type.Fun(t1s2, t1'))) (Type.Fun(t2s, t2'))
  | Type.Fun(t1s, t1'), Type.Fun(t2s, t2') ->
      (try List.iter2 unify t1s t2s
      with Invalid_argument("List.iter2") -> raise (Unify(t1, t2)));
      unify t1' t2'
  | Type.Tuple(t1s), Type.Tuple(t2s) ->
      (try List.iter2 unify t1s t2s
      with Invalid_argument("List.iter2") -> raise (Unify(t1, t2)))
  | Type.Array(t1), Type.Array(t2) -> unify t1 t2
  | Type.Var(r1), Type.Var(r2) when r1 == r2 -> ()
  | Type.Var({ contents = Some(t1') }), _ -> unify t1' t2
  | _, Type.Var({ contents = Some(t2') }) -> unify t1 t2'
  | Type.Var({ contents = None } as r1), _ -> (* 一方が未定義の型変数の場合 *)
      if occur r1 t2 then raise (Unify(t1, t2));
      r1 := Some(t2)
  | _, Type.Var({ contents = None } as r2) ->
      if occur r2 t1 then raise (Unify(t1, t2));
      r2 := Some(t1)
  | Type.List({ contents = Some(t1) }), Type.List({ contents = None } as r2) ->
      r2 := Some(t1)
  | Type.List({ contents = None } as r1), Type.List({ contents = Some(t2) }) ->
      r1 := Some(t2)
  | Type.List(r1), Type.List(r2) when r1 == r2 -> ()
  | Type.List({ contents = None }), Type.List({ contents = None }) -> () (* TODO: this is correct? *)
  | Type.List({ contents = Some(t1) }), Type.List({ contents = Some(t2) }) -> unify t1 t2
  | _, _ -> raise (Unify(t1, t2))


let rec g env e = (* 型推論ルーチン *)
  try
    match e with
      | Unit -> Type.Unit
      | Bool(_) -> Type.Bool
      | Int(_) -> Type.Int
      | Float(_) -> Type.Float
      | Not(e) ->
        unify Type.Bool (g env e);
        Type.Bool
      | Neg(e) | Sll(e, _) | Sra(e, _) ->
        unify Type.Int (g env e);
        Type.Int
      | Add(e1, e2) | Sub(e1, e2) | Mul(e1, e2) -> (* 足し算等の型推論 *)
        unify Type.Int (g env e1);
        unify Type.Int (g env e2);
        Type.Int
      | FNeg(e) ->
        unify Type.Float (g env e);
        Type.Float
      | FAdd(e1, e2) | FSub(e1, e2) | FMul(e1, e2) | FDiv(e1, e2) ->
        unify Type.Float (g env e1);
        unify Type.Float (g env e2);
        Type.Float
      | Eq(e1, e2) | LE(e1, e2) | LT(e1, e2) ->
        unify (g env e1) (g env e2);
        Type.Bool
      | If(e1, e2, e3) ->
        unify (g env e1) Type.Bool;
        let t2 = g env e2 in
        let t3 = g env e3 in
        unify t2 t3;
        t2
      | Let((x, t), e1, e2) -> (* letの型推論 *)
        unify t (g env e1);
        g (M.add x t env) e2
      | Var(x) when M.mem x env -> M.find x env (* 変数の型推論 *)
      | Var(x) when M.mem x !extenv -> M.find x !extenv
      | Var(x) -> (* 外部変数の型推論 *)
        Format.eprintf "free variable %s assumed as external@." x;
        let t = Type.gentyp () in
        extenv := M.add x t !extenv;
        t
      | LetRec({name = (x,t); args = yts; body = e1},e2) -> (* let recの型推論 *)
        let env = M.add x t env in
        unify t (Type.Fun(List.map snd yts, g (M.add_list yts env) e1));
        g env e2
      | App(e, es) -> (* 関数適用の型推論 *)
        let t = Type.gentyp () in
        unify (g env e) (Type.Fun(List.map (g env) es, t));
        t
      | Tuple(es) -> Type.Tuple(List.map (g env) es)
      | LetTuple(xts, e1, e2) ->
        unify (Type.Tuple(List.map snd xts)) (g env e1);
        g (M.add_list xts env) e2
      | Array(e1, e2) -> (* must be a primitive for "polymorphic" typing *)
        unify (g env e1) Type.Int;
        Type.Array(g env e2)
      | Get(e1, e2) ->
        let t = Type.gentyp () in
        unify (Type.Array(t)) (g env e1);
        unify Type.Int (g env e2);
        t
      | Put(e1, e2, e3) ->
        let t = g env e3 in
        unify (Type.Array(t)) (g env e1);
        unify Type.Int (g env e2);
        Type.Unit
      | Match(e1, cases) ->                (* currently, only support INT *)
        unify Type.Int (g env e1);
        let types = List.map (fun (pattern, e) -> g env e) cases in
        let first_type = List.hd types in
        List.iter (unify first_type) types; (* First check is redundant *)
        first_type
      | Nil -> Type.List(ref None)
      | IsNil(e) -> unify (Type.List(ref None)) (g env e); Type.Bool
      | Cons(head, tail) ->
        let type_of_head = g env head in
        let type_of_tail = g env tail in
        unify (Type.List(ref (Some(type_of_head)))) type_of_tail;
        type_of_tail
      | LetList((matcher, typ), e1, e2) ->
        let body_type = (g env e1) in
        unify (Type.List typ) body_type;
        g (M.add_list_matcher matcher typ env) e2

  with Unify(t1, t2) ->
    Printf.eprintf "Unify Error : In %s\n%!";
    Printf.eprintf "%s\n" (Show.show<Syntax.t> e);
    Printf.eprintf "Unify(%s, %s)\n%!" (Type.show t1) (Type.show t2);
    raise (Error(deref_term e, deref_typ t1, deref_typ t2))

let f e =
  extenv := M.empty;
  (try unify Type.Unit (g M.empty e)
   with Unify _ -> Printf.eprintf "Warning : top level does not have type unit\n");
  extenv := M.map deref_typ !extenv;
  deref_term e
