open Asm

let ze env x = if M.mem x env && M.find x env = 0 then "$r0" else x 

let rec g env = function (* 命令列の16bit即値最適化 *)
  | Ans(exp) -> Ans(g' env exp)
  | Let((x, t), Int(i), e) when -0x8000 <= i && i < 0x7FFF ->
      (* Format.eprintf "found simm16 %s = %d@." x i; *)
      let e' = g (M.add x i env) e in
      if List.mem x (fv e') then Let((x, t), Int(i), e') else
      ((* Format.eprintf "erased redundant Set to %s@." x; *)
       e')
  | Let(xt, exp, e) -> Let(xt, g' env exp, g env e)
and g' env = function (* 各命令の16bit即値最適化 *)
  | Add(x, y) when M.mem y env -> AddI(x, M.find y env)
  | Sub(x, y) when M.mem y env -> SubI(x, M.find y env)
  | Mul(x, y) when M.mem y env -> MulI(x, M.find y env)
  | And(x, y) when M.mem y env -> AndI(x, M.find y env)
  | Or (x, y) when M.mem y env -> OrI (x, M.find y env)
  | Nor(x, y) when M.mem y env -> NorI(x, M.find y env)
  | Xor(x, y) when M.mem y env -> XorI(x, M.find y env)

  | LdR (x, y) when M.mem y env -> LdI (x, M.find y env)
  | FLdR(x, y) when M.mem y env -> FLdI(x, M.find y env)

  | IfEq (x, y, e1, e2) -> IfEq(ze env x, ze env y, g env e1, g env e2)
  | IfLE (x, y, e1, e2) -> IfLE(ze env x, ze env y, g env e1, g env e2)
  | IfLT (x, y, e1, e2) -> IfLT(ze env x, ze env y, g env e1, g env e2)
  | IfFEq(x, y, e1, e2) -> IfFEq(x, y, g env e1, g env e2)
  | IfFLE(x, y, e1, e2) -> IfFLE(x, y, g env e1, g env e2)
  | IfFLT(x, y, e1, e2) -> IfFLT(x, y, g env e1, g env e2)
  | e -> e

let h { name = l; args = xs; fargs = ys; body = e; ret = t } = (* トップレベル関数の16bit即値最適化 *)
  { name = l; args = xs; fargs = ys; body = g M.empty e; ret = t }

let f (Prog(fundefs, e)) = (* プログラム全体の16bit即値最適化 *)
  Prog(List.map h fundefs, g M.empty e)
