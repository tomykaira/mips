open ANormal

(* インライン展開する関数の最大サイズ. Mainで-inlineオプションによりセットされる *)
let threshold = ref 0

(* その関数が末尾再帰か判定 *)
let rec tailrec x = function
  | Let(_,_,e) | LetRec(_,e) | LetTuple(_,_,e) | LetList(_,_,e) -> tailrec x e
  | Ans(exp) -> tailrec' x exp
and tailrec' x = function
  | IfEq(_, _, e1, e2) | IfLE(_, _, e1, e2) | IfLT(_, _, e1, e2) | IfNil(_, e1, e2) 
    -> tailrec x e1 || tailrec x e2
  | App(y,_) when x = y -> true
  | _ -> false

let rec g env = function (* インライン展開ルーチン本体 *)
  | Let(xt, exp, e) -> concat (g' env exp) xt (g env e)
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> (* 関数定義の場合 *)
    let env = if size e1 > !threshold || tailrec x e1 then env
    else M.add x (yts, e1) env in
    LetRec({ name = (x, t); args = yts; body = g env e1}, g env e2)
  | LetTuple(xts, y, e) -> LetTuple(xts, y, g env e)
  | LetList(xts, y, e) -> LetList(xts, y, g env e)
  | Ans(exp) -> g' env exp
and g' env = function
  | IfEq(x, y, e1, e2) -> Ans(IfEq(x, y, g env e1, g env e2))
  | IfLE(x, y, e1, e2) -> Ans(IfLE(x, y, g env e1, g env e2))
  | IfLT(x, y, e1, e2) -> Ans(IfLT(x, y, g env e1, g env e2))
  | IfNil(x, e1, e2) -> Ans(IfNil(x, g env e1, g env e2))
  | App(x, ys) when M.mem x env -> (* 関数適用の場合 *)
    let (zs, e) = M.find x env in
    let env' =
      List.fold_left2
        (fun env' (z, _) y -> M.add z y env')
        M.empty
        zs
        ys in
    ag env' e
  | e -> Ans(e)


let f e =
  Format.eprintf "inlining functions...@.";
  g M.empty e
