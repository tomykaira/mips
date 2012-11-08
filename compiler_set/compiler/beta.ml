open KNormal

let find x env = try M.find x env with Not_found -> x (* 置換のための関数 *)

let rec g env = function (* β簡約ルーチン本体 *)
  | Unit -> Unit
  | Int(i) -> Int(i)
  | Float(d) -> Float(d)
  | Neg(x) -> Neg(find x env)
  | Add(x, y) -> Add(find x env, find y env)
  | Sub(x, y) -> Sub(find x env, find y env)
  | Mul(x, y) -> Mul(find x env, find y env)
  | Sll(x, y) -> Sll(find x env, y)
  | Sra(x, y) -> Sra(find x env, y)
  | FNeg(x) -> FNeg(find x env)
  | FAdd(x, y) -> FAdd(find x env, find y env)
  | FSub(x, y) -> FSub(find x env, find y env)
  | FMul(x, y) -> FMul(find x env, find y env)
  | FDiv(x, y) -> FDiv(find x env, find y env)
  | IfEq(x, y, e1, e2) -> IfEq(find x env, find y env, g env e1, g env e2)
  | IfLE(x, y, e1, e2) -> IfLE(find x env, find y env, g env e1, g env e2)
  | IfLT(x, y, e1, e2) -> IfLT(find x env, find y env, g env e1, g env e2)
  | IfNil(x, e1, e2) -> IfNil(find x env, g env e1, g env e2)
  | Let((x, t), e1, e2) -> (* letのβ簡約 *)
      (match g env e1 with
      | Var(y) ->
	  Format.eprintf "beta-reducing %s = %s@." x y;
	  g (M.add x y env) e2
      | e1' ->
	  let e2' = g env e2 in
	  Let((x, t), e1', e2'))
  | LetRec({ name = xt; args = yts; body = e1 }, e2) ->
      LetRec({ name = xt; args = yts; body = g env e1 }, g env e2)
  | Var(x) -> Var(find x env) (* 変数を置換 *)
  | Tuple(xs) -> Tuple(List.map (fun x -> find x env) xs)
  | LetTuple(xts, y, e) -> LetTuple(xts, find y env, g env e)
  | Get(x, y) -> Get(find x env, find y env)
  | Put(x, y, z) -> Put(find x env, find y env, find z env)
  | App(g, xs) -> App(find g env, List.map (fun x -> find x env) xs)
  | ExtArray(x) -> ExtArray(x)
  | ExtFunApp(x, ys) -> ExtFunApp(x, List.map (fun y -> find y env) ys)
  | Nil -> Nil
  | Cons(x, y) -> Cons(find x env, find y env)
  | LetList(xts, y, e) -> LetList(xts, find y env, g env e)

let f = g M.empty
