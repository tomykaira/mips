open Asm

(* 定数畳み込みを行うモジュール。整数についてのみ *)


(* 定数を畳み込む関数 *)
let rec g' env = function
  | Add(x, y) when M.mem x env && M.mem y env ->
      Int(M.find x env + M.find y env)
  | Sub(x, y) when M.mem x env && M.mem y env ->
      Int(M.find x env - M.find y env)
  | Mul(x, y) when M.mem x env && M.mem y env ->
      Int(M.find x env * M.find y env)
  | And(x, y) when M.mem x env && M.mem y env ->
      Int((M.find x env) land (M.find y env))
  | Or (x, y) when M.mem x env && M.mem y env ->
      Int((M.find x env) lor (M.find y env))
  | Nor(x, y) when M.mem x env && M.mem y env ->
      Int(lnot ((M.find x env) lor (M.find y env)))
  | Xor(x, y) when M.mem x env && M.mem y env ->
      Int((M.find x env) lxor (M.find y env))

  | AddI(x, i) when M.mem x env -> Int(M.find x env + i)
  | SubI(x, i) when M.mem x env -> Int(M.find x env - i)
  | MulI(x, i) when M.mem x env -> Int(M.find x env * i)
  | AndI(x, i) when M.mem x env -> Int((M.find x env) land i)
  | OrI (x, i) when M.mem x env -> Int((M.find x env) lor i)
  | NorI(x, i) when M.mem x env -> Int(lnot((M.find x env) lor i))
  | XorI(x, i) when M.mem x env -> Int((M.find x env) lxor i)

  | SllI(x, i) when M.mem x env -> Int((M.find x env) lsl i)
  | SraI(x, i) when M.mem x env -> Int((M.find x env) asr i)

  | IfEq(x, y, e1, e2) -> IfEq(x, y, g env e1, g env e2)
  | IfLE(x, y, e1, e2) -> IfLE(x, y, g env e1, g env e2)
  | IfLT(x, y, e1, e2) -> IfLT(x, y, g env e1, g env e2)
  | IfFEq(x, y, e1, e2) -> IfFEq(x, y, g env e1, g env e2)
  | IfFLE(x, y, e1, e2) -> IfFLE(x, y, g env e1, g env e2)
  | IfFLT(x, y, e1, e2) -> IfFLT(x, y, g env e1, g env e2)

  | e -> e

and g env = function
  | Let(xt, IfEq(x, y, e1, e2), e3) when M.mem x env && M.mem y env -> h (=) x y env xt e1 e2 e3
  | Let(xt, IfLE(x, y, e1, e2), e3) when M.mem x env && M.mem y env -> h (<=) x y env xt e1 e2 e3
  | Let(xt, IfLT(x, y, e1, e2), e3) when M.mem x env && M.mem y env -> h (<) x y env xt e1 e2 e3

  | Let((x,t), e1, e2) ->
      let e1' = g' env e1 in
      (match e1' with
      | Int(i) | AddI("$r0", i) -> let e2' = if is_reg x then e2 else g (M.add x i env) e2 in
	          if List.mem x (fv e2) then Let((x, t), e1', e2') else e2'
      | _ -> Let((x, t), e1', g env e2))
  | Ans(e) -> Ans(g' env e)
and h k x y env xt e1 e2 e3 =
  let e = if k (M.find x env) (M.find y env) then g env e1 else g env e2 in    
  (match e with
  | Ans(exp) -> g env (Let(xt, exp, e3))
  | _ -> concat e xt e3)
			     

let f (Prog(toplevel, e)) =
  Format.eprintf "const folding for assembly...@.";
  Prog(List.map (fun { name = l; args = ys; fargs = zs; body = e; ret = t } ->
    { name = l; args = ys; fargs = zs; body = g M.empty e; ret = t }) toplevel,
    g M.empty e)
