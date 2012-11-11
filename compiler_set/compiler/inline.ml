open ANormal

(* インライン展開する関数の最大サイズ. Mainで-inlineオプションによりセットされる *)
let threshold = ref 0 

let rec size = function
  | Let(_, exp, e) -> 1 + size' exp + size e
  | LetRec({ body = e1 }, e2) -> 1 + size e1 + size e2
  | LetTuple(_, _, e) -> 1 + size e
  | Ans(exp) -> size' exp
and size' = function
  | IfEq(_, _, e1, e2) | IfLE(_, _, e1, e2) | IfLT(_, _, e1, e2)
    -> 1 + size e1 + size e2
  | _ -> 1


let find x env = try M.find x env with Not_found -> x
(* α変換ルーチン *)
let rec ag env = function 
  | Let((x, t), exp, e) -> 
      let x' = Id.genid x in
      Let((x', t), ag' env exp, ag (M.add x x' env) e)
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> 
      let env = M.add x (Id.genid x) env in
      let ys = List.map fst yts in
      let env' = M.add_list2 ys (List.map Id.genid ys) env in
      LetRec({ name = (find x env, t);
	       args = List.map (fun (y, t) -> (find y env', t)) yts;
	       body = ag env' e1 },
	     ag env e2)
  | LetTuple(xts, y, e) -> 
      let xs = List.map fst xts in
      let env' = M.add_list2 xs (List.map Id.genid xs) env in
      LetTuple(List.map (fun (x, t) -> (find x env', t)) xts,
	       find y env,
	       ag env' e)
  | Ans(exp) -> Ans(ag' env exp)
and ag' env = function
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
  | IfEq(x, y, e1, e2) -> IfEq(find x env, find y env, ag env e1, ag env e2)
  | IfLE(x, y, e1, e2) -> IfLE(find x env, find y env, ag env e1, ag env e2)
  | IfLT(x, y, e1, e2) -> IfLT(find x env, find y env, ag env e1, ag env e2)
  | Var(x) -> Var(find x env)
  | App(x, ys) -> App(find x env, List.map (fun y -> find y env) ys)
  | Tuple(xs) -> Tuple(List.map (fun x -> find x env) xs)
  | Get(x, y) -> Get(find x env, find y env)
  | Put(x, y, z) -> Put(find x env, find y env, find z env)
  | ExtArray(x) -> ExtArray(x)
  | ExtFunApp(x, ys) -> ExtFunApp(x, List.map (fun y -> find y env) ys)


(* インライン展開ルーチン本体 *)
let rec g env = function 
  | Let(xt, exp, e) -> concat (g' env exp) xt (g env e)
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> (* 関数定義の場合 *)
      let env = if size e1 > !threshold then env else M.add x (yts, e1) env in
      LetRec({ name = (x, t); args = yts; body = g env e1}, g env e2)
  | LetTuple(xts, y, e) -> LetTuple(xts, y, g env e)
  | Ans(exp) -> g' env exp
and g' env = function
  | IfEq(x, y, e1, e2) -> Ans(IfEq(x, y, g env e1, g env e2))
  | IfLE(x, y, e1, e2) -> Ans(IfLE(x, y, g env e1, g env e2))
  | IfLT(x, y, e1, e2) -> Ans(IfLT(x, y, g env e1, g env e2))
  | App(x, ys) when M.mem x env -> (* 関数適用の場合 *)
      let (zs, e) = M.find x env in
      let env' =
	List.fold_left2
	  (fun env' (z, t) y -> M.add z y env')
	  M.empty
	  zs
	  ys in
      ag env' e
  | e -> Ans(e)

let f e = Format.eprintf "inlining functions...@.";
          g M.empty e
