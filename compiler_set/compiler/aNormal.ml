(*pp deriving *)

(* A正規化を行うモジュール *)

type t = (* let列 *)
  | Let of (Id.t * Type.t) * exp * t
  | LetRec of fundef * t
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | LetList of (Syntax.list_matcher * Type.t) * Id.t * t
  | Ans of exp
and exp = (* 式 *)
  | Unit
  | Int of int
  | Float of float
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | Mul of Id.t * Id.t
  | Sll of Id.t * int
  | Sra of Id.t * int
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | IfEq of Id.t * Id.t * t * t (* 比較 + 分岐 *)
  | IfLE of Id.t * Id.t * t * t (* 比較 + 分岐 *)
  | IfLT of Id.t * Id.t * t * t (* 比較 + 分岐 *)
  | IfNil of Id.t * t * t (* 比較 + 分岐 *)
  | Var of Id.t
  | App of Id.t * Id.t list
  | Tuple of Id.t list
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.t
  | ExtFunApp of Id.t * Id.t list
  | Nil
  | Cons of Id.t * Id.t
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }
    deriving (Show)

let rec concat e1 xt e2 =
  match e1 with
  | Let(yt, e3, e4) -> Let(yt, e3, concat e4 xt e2)
  | LetRec(fundefs, e) -> LetRec(fundefs, concat e xt e2)
  | LetTuple(yts, z, e) -> LetTuple(yts, z, concat e xt e2)
  | LetList(yt, z, e) -> LetList(yt, z, concat e xt e2)
  | Ans(exp) -> Let(xt, exp, e2)


let rec fv = function (* 式に出現する（自由な）変数 *)
  | Let((x, _), exp, e) -> S.union (fv' exp) (S.remove x (fv e))
  | LetRec({ name = (x, _); args = yts; body = e1 }, e2) ->
      let zs = S.diff (fv e1) (S.of_list (List.map fst yts)) in
      S.diff (S.union zs (fv e2)) (S.singleton x)
  | LetTuple(xs, y, e) -> S.add y (S.diff (fv e) (S.of_list (List.map fst xs)))
  | LetList((matcher, _), y, e) ->
    S.add y (S.diff (fv e) (S.of_list (Syntax.matcher_variables matcher)))
  | Ans(exp) -> fv' exp
and fv' = function
  | Unit | Nil | Int(_) | Float(_) | ExtArray(_) -> S.empty
  | Neg(x) | FNeg(x) | Sll(x, _) | Sra(x, _) -> S.singleton x
  | Add(x, y) | Sub(x, y) | Mul(x, y) | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) | Get(x, y) | Cons(x, y) -> S.of_list [x; y]
  | IfEq(x, y, e1, e2) | IfLE(x, y, e1, e2) | IfLT(x, y, e1, e2) -> S.add x (S.add y (S.union (fv e1) (fv e2)))
  | IfNil(x, e1, e2) -> S.add x (S.union (fv e1) (fv e2))
  | Var(x) -> S.singleton x
  | App(x, ys) -> S.of_list (x :: ys)
  | Tuple(xs) | ExtFunApp(_, xs) -> S.of_list xs
  | Put(x, y, z) -> S.of_list [x; y; z]

let rec btoi = function (* 型の中のBool型をInt型に置き換える補助関数 *)
  | Type.Bool -> Type.Int
  | Type.Fun(a,b) -> Type.Fun(List.map btoi a, btoi b)
  | Type.Tuple(a) -> Type.Tuple(List.map btoi a)
  | Type.Array(a) -> Type.Array(btoi a)
  | Type.Var(a) as t-> (match !a with Some(b) -> a := Some(btoi b) | _ -> ()); t
  | Type.List(a) as t-> (match !a with Some(b) -> a := Some(btoi b) | _ -> ()); t
  | t -> t
let bis (x,t) = (x, btoi t)


(* ネストしたletの簡約 *)
let rec f = function 
  | KNormal.Let(xt, e1, e2) -> concat (f e1) (bis xt) (f e2)
  | KNormal.LetRec({ KNormal.name = xt; KNormal.args = yts; KNormal.body = e1 }, e2) ->
      LetRec({ name = bis xt; args = List.map bis yts; body = f e1 }, f e2)
  | KNormal.LetTuple(xts, y, e) -> LetTuple(List.map bis xts, y, f e)
  | KNormal.LetList(xt, y, e) -> LetList(bis xt, y, f e)
  | e -> Ans(f' e)
and f' = function
  | KNormal.Unit -> Unit
  | KNormal.Int(i) -> Int(i)
  | KNormal.Float(d) -> Float(d)
  | KNormal.Neg(x) -> Neg(x)
  | KNormal.Add(x, y) -> Add(x, y)
  | KNormal.Sub(x, y) -> Sub(x, y)
  | KNormal.Mul(x, y) -> Mul(x, y)
  | KNormal.Sll(x, y) -> Sll(x, y)
  | KNormal.Sra(x, y) -> Sra(x, y)
  | KNormal.FNeg(x) -> FNeg(x)
  | KNormal.FAdd(x, y) -> FAdd(x, y)
  | KNormal.FSub(x, y) -> FSub(x, y)
  | KNormal.FMul(x, y) -> FMul(x, y)
  | KNormal.FDiv(x, y) -> FDiv(x, y)
  | KNormal.IfEq(x, y, e1, e2) -> IfEq(x, y, f e1, f e2)
  | KNormal.IfLE(x, y, e1, e2) -> IfLE(x, y, f e1, f e2)
  | KNormal.IfLT(x, y, e1, e2) -> IfLT(x, y, f e1, f e2)
  | KNormal.IfNil(x, e1, e2) -> IfNil(x, f e1, f e2)
  | KNormal.Var(x) -> Var(x)
  | KNormal.App(x, ys) -> App(x, ys)
  | KNormal.Tuple(xs) -> Tuple(xs)
  | KNormal.Get(x, y) -> Get(x, y)
  | KNormal.Put(x, y, z) -> Put(x, y, z)
  | KNormal.ExtArray(x) -> ExtArray(x)
  | KNormal.ExtFunApp(x, ys) -> ExtFunApp(x, ys)
  | KNormal.Nil -> Nil
  | KNormal.Cons(x,y) -> Cons(x,y)
  | _ -> assert false


let er find x env = try find x env with Not_found -> Format.eprintf "%s : Not_found@." x; failwith "Not_found"
