open ANormal

(* 共通部分式除去を行うモジュール *)

let qlen = 10

let add x l = if List.mem x l then l else x::l

let addq x l =
  let l' = add x l in if List.length l' > qlen then List.tl l' else l'
let push l = addq (Var(Id.genid ""), "%g0") l
let rec pushn n l = if n<=0 then l else pushn (n-1) (push l)

(* 部分式をインデックスに変数を引く関数 *)
let rec find x env = try Var(List.assoc x env) with Not_found -> x

(* 共通部分式除去を行う関数 *)
let rec g env env2 env3 = function
  | Let((x,_) as xt, exp, e) ->
      let exp' = find (g' env env2 env3 exp) (env@env3) in
      (match exp' with
      | Unit | Var _ | Get _ | Put _ | Tuple _ | ExtArray _ | Nil | Cons _ -> 
	  Let(xt, exp', g env env2 (push env3) e)
      | Int i when -0x8000 <= i && i <= 0x7FFF ->
	  Let(xt, exp', g env env2 (addq (exp', x) env3) e)
      | App(y, _) ->
	  if S.mem y env2 then Let(xt, exp', g [] env2 [] e)
	  else Let(xt, exp', g [(exp', x)] env2 [] e)
      | ExtFunApp _ -> Let(xt, exp', g [] env2 [] e)
      | _ -> Let(xt, exp', g (add (exp',x) env) env2 (pushn (Inline.size' exp') env3) e))
  | LetRec({ name = (x,_) as xt; args = yts; body = e1 }, e2) ->
      let env2' =
	let env2' = S.add x env2 in
	if Elim.effect env2' e1 then env2 else env2' in
      LetRec({ name = xt; args = yts; body = g [] env2' env3 e1 }, g env env2' (push env3) e2)
  | LetTuple(l, y, e) ->
      LetTuple(l, y, g env env2 (pushn (List.length l) env3) e)
  | LetList((x,_) as xt, y, e) ->
      LetList(xt, y, g env env2 (pushn (List.length (Syntax.matcher_variables x)) env3) e)
  | Ans(exp) -> Ans(g' env env2 env3 exp)
and g' env env2 env3 = function
  | IfEq(x, y, e1, e2) -> IfEq(x, y, g env env2 env3 e1, g env env2 env3 e2)
  | IfLE(x, y, e1, e2) -> IfLE(x, y, g env env2 env3 e1, g env env2 env3 e2)
  | IfLT(x, y, e1, e2) -> IfLT(x, y, g env env2 env3 e1, g env env2 env3 e2)
  | IfNil(x, e1, e2) -> IfNil(x, g env env2 env3 e1, g env env2 env3 e2)
  | e -> e
	

let f e = Format.eprintf "eliminating common subexpressions...@.";
          g [] S.empty [] e
