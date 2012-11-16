open Asm

let co env x = if M.mem x env && M.find x env = 0 then reg_0 
               else if M.mem x env && M.find x env = 1 then reg_1 
               else if M.mem x env && M.find x env = -1 then reg_m1 else x

let rec g env = function (* 命令列の16bit即値最適化 *)
  | Ans(exp) -> Ans(g' env exp)
  | Let((x, t), Int(i), e) when -0x8000 <= i && i < 0x7FFF ->
      let e' = g (M.add x i env) e in
      if List.mem x (fv e') then Let((x, t), Int(i), e') else
      e'
  | Let((x, t), Float(0.0), e) ->
      let e' = g (M.add x 0 env) e in
      if List.mem x (fv e') then Let((x, t), Float(0.0), e') else
      e'      
  | Let(xt, exp, e) -> Let(xt, g' env exp, g env e)
and g' env = function (* 各命令の16bit即値最適化 *)
  | Add(x, y) when M.mem y env -> AddI(x, M.find y env)
  | Add(x, y) when M.mem x env -> AddI(y, M.find x env)
  | Sub(x, y) when M.mem y env -> SubI(x, M.find y env)
  | Mul(x, y) when M.mem y env -> MulI(x, M.find y env)
  | Mul(x, y) when M.mem x env -> MulI(y, M.find x env)
  | And(x, y) when M.mem y env -> AndI(x, M.find y env)
  | And(x, y) when M.mem x env -> AndI(y, M.find x env)
  | Or (x, y) when M.mem y env -> OrI (x, M.find y env)
  | Or (x, y) when M.mem x env -> OrI (y, M.find x env)
  | Nor(x, y) when M.mem y env -> NorI(x, M.find y env)
  | Nor(x, y) when M.mem x env -> NorI(y, M.find x env)
  | Xor(x, y) when M.mem y env -> XorI(x, M.find y env)
  | Xor(x, y) when M.mem x env -> XorI(y, M.find x env)

  | LdR (x, y) when M.mem y env -> LdI (x, M.find y env)
  | LdR (x, y) when M.mem x env -> LdI (y, M.find x env)
  | FLdR(x, y) when M.mem y env -> FLdI(x, M.find y env)
  | FLdR(x, y) when M.mem x env -> FLdI(y, M.find x env)

  | StI(x, y, z) -> StI(co env x, y, z)
  | FStI(x, y, z) when M.mem x env && M.find x env = 0 -> StI(reg_0, y, z)

  | IfEq (x, y, e1, e2) -> IfEq(co env x, co env y, g env e1, g env e2)
  | IfLE (x, y, e1, e2) -> IfLE(co env x, co env y, g env e1, g env e2)
  | IfLT (x, y, e1, e2) -> IfLT(co env x, co env y, g env e1, g env e2)
  | IfFEq(x, y, e1, e2) -> IfFEq(x, y, g env e1, g env e2)
  | IfFLE(x, y, e1, e2) -> IfFLE(x, y, g env e1, g env e2)
  | IfFLT(x, y, e1, e2) -> IfFLT(x, y, g env e1, g env e2)
  | e -> e

(* トップレベル関数の16bit即値最適化 *)
let h { name = l; args = xs; fargs = ys; body = e; ret = t } = 
  { name = l; args = xs; fargs = ys; body = g M.empty e; ret = t }

(* プログラム全体の16bit即値最適化 *)
let f (Prog(fundefs, e)) = 
  Prog(List.map h fundefs, g M.empty e)
