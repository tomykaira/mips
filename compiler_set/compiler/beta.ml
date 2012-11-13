open ANormal

let find x env = try M.find x env with Not_found -> x (* 置換のための関数 *)

let rec g env = function (* β簡約ルーチン本体 *)
  | Let((x, t), exp, e) -> (* letのβ簡約 *)
    (match g' env exp with
      | Var(y) ->
        g (M.add x y env) e
      | exp' ->
        let e' = g env e in
        Let((x, t), exp', e'))
  | LetRec({ name = xt; args = yts; body = e1 }, e2) ->
    LetRec({ name = xt; args = yts; body = g env e1 }, g env e2)
  | LetTuple(xts, y, e) -> LetTuple(xts, find y env, g env e)
  | LetList(xts, y, e) -> LetList(xts, find y env, g env e)
  | Ans(exp) -> Ans(g' env exp)
and g' env = function
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
  | IfEq(x, y, e1, e2) ->
      let x' = find x env in
      let y' = find y env in
      IfEq(x', y', g (M.add x' y' env) e1, g env e2)
  | IfLE(x, y, e1, e2) -> IfLE(find x env, find y env, g env e1, g env e2)
  | IfLT(x, y, e1, e2) -> IfLT(find x env, find y env, g env e1, g env e2)
  | IfNil(x, e1, e2) -> IfNil(find x env, g env e1, g env e2)
  | Var(x) -> Var(find x env) (* 変数を置換 *)
  | Tuple(xs) -> Tuple(List.map (fun x -> find x env) xs)
  | Get(x, y) -> Get(find x env, find y env)
  | Put(x, y, z) -> Put(find x env, find y env, find z env)
  | App(g, xs) -> App(find g env, List.map (fun x -> find x env) xs)
  | ExtArray(x) -> ExtArray(x)
  | ExtFunApp(x, ys) -> ExtFunApp(x, List.map (fun y -> find y env) ys)
  | Nil -> Nil
  | Cons(x, y) -> Cons(find x env, find y env)


let f = g M.empty




let add x y env = if x = y then env else M.add x y env
let add_list l env = List.fold_left (fun env (x,y) -> add x y env) env l 
(* 2つのプログラムが変数名の違いを除いて同じか判定する関数 *)
let rec same env e1 e2 =
  match (e1, e2) with
  | Let((x1,t1),exp1,e1'), Let((x2,t2),exp2,e2') when same' env exp1 exp2 && t1 = t2 ->
      same (add x1 x2 env) e1' e2'
  | LetRec({ name = (x1, t1); args = yts1; body = e1' }, e1''), LetRec({ name = (x2, t2); args = yts2; body = e2' }, e2'') when t1 = t2 && List.map snd yts1 = List.map snd yts2 ->
      let env' = add x1 x2 env in
      same (add_list (List.fold_left2 (fun l (a,_) (b,_) -> (a,b)::l) [] yts1 yts2) env') e1' e2' && same env' e1'' e2''
  | LetTuple(xts1, y1, e1'), LetTuple(xts2, y2, e2') when List.map snd xts1 = List.map snd xts2 && find y1 env = y2 ->
      same (List.fold_left2 (fun env (a,_) (b,_) -> add a b env) env xts1 xts2) e1' e2'
  | LetList((x1,t1), y1, e1'), LetList((x2,t2), y2, e2') when t1 = t2 && find y1 env = y2 ->
      same (List.fold_left2 (fun env a b -> add a b env) env (Syntax.matcher_variables x1) (Syntax.matcher_variables x2)) e1' e2'
  | Ans(exp1), Ans(exp2) -> same' env exp1 exp2
  | _ -> false
and same' env exp1 exp2 =
  match (exp1, exp2) with
  | IfEq(x1,y1,e1,e1'), IfEq(x2,y2,e2,e2') | IfLE(x1,y1,e1,e1'), IfLE(x2,y2,e2,e2') | IfLT(x1,y1,e1,e1'), IfLT(x2,y2,e2,e2') ->
      find x1 env = x2 && find y1 env = y2 && same env e1 e2 && same env e1' e2'
  | IfNil(x1,e1,e1'), IfNil(x2,e2,e2') ->
      find x1 env = x2 && same env e1 e2 && same env e1' e2'
  | Float i, Float j ->
      (match classify_float i, classify_float j with
      | FP_infinite, FP_infinite | FP_nan, FP_nan -> true
      | _ -> i = j)
  | _ -> g' env exp1 = exp2 
	 
