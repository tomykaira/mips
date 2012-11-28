open Asm

(* 16bitの即値最適化と定数畳み込みを行うモジュール *)

let co env x = if M.mem x env && M.find x env = 0 then reg_0 
               else if M.mem x env && M.find x env = 1 then reg_1 
               else if M.mem x env && M.find x env = -1 then reg_m1 else x

let fco env x = if M.mem x env && M.find x env = 0 then reg_f0 else x

(* その数が2の何乗か返す *)
let rec log2_sub n i =
  if 1 lsl (i+1) > n then i
  else log2_sub n (i+1)
let log2 n = log2_sub n 0
(* その数が2のべき乗か返す *)
let is_bin n = 1 lsl (log2 n) = n

let is16 i = -0x8000 <= i && i <= 0x7FFF

let rec g env = function (* 命令列の16bit即値最適化 *)
  | Ans(exp) -> g' env exp    
  | Let((x, t) as xt, exp, e) ->
      let exp' = g' env exp in
      let (b, env') = match exp' with
      | Ans(Int(i)) -> (true, M.add x i env)
      | Ans(AddI(y, 0)) when M.mem y env -> (true, M.add x (M.find y env) env)
      | Ans(Float(0.0)) -> (true, M.add x 0 env)
      | _ -> (false, env) in
      let e' = g env' e in
      if b && not (List.mem x (fv e')) then e'
      else concat exp' xt e'
and g' env = function (* 各命令の16bit即値最適化 *)
  | Add(x, y) when M.mem x env && M.mem y env ->
      Ans(Int(M.find x env + M.find y env))
  | Sub(x, y) when M.mem x env && M.mem y env ->
      Ans(Int(M.find x env - M.find y env))
  | Xor(x, y) when M.mem x env && M.mem y env ->
      Ans(Int((M.find x env) lxor (M.find y env)))

  | AddI(x, i) when M.mem x env -> Ans(Int(M.find x env + i))
  | AddI(x, i) when x = reg_0 -> Ans(Int(i))
  | SubI(x, i) when M.mem x env -> Ans(Int(M.find x env - i))
  | XorI(x, i) when M.mem x env -> Ans(Int((M.find x env) lxor i))

  | SllI(x, i) when M.mem x env && Int32.shift_left (Int32.of_int (M.find x env)) i = Int32.of_int ((M.find x env) lsl i) ->
      Ans(Int((M.find x env) lsl i))
  | SraI(x, i) when M.mem x env ->
      Ans(Int((M.find x env) asr i))



  | Add(x, y) when M.mem y env && is16 (M.find y env) ->
      Ans(AddI(x, M.find y env))
  | Add(x, y) when M.mem x env && is16 (M.find x env) ->
      Ans(AddI(y, M.find x env))
  | Sub(x, y) when M.mem y env && is16 (M.find y env) ->
      Ans(SubI(x, M.find y env))
  | Xor(x, y) when M.mem y env && is16 (M.find y env) ->
      Ans(XorI(x, M.find y env))
  | Xor(x, y) when M.mem x env && is16 (M.find x env) ->
      Ans(XorI(y, M.find x env))

  | FMov(x) -> Ans(FMov(fco env x))
  | FNeg(x) -> Ans(FNeg(fco env x))
  | FAdd(x, y) -> Ans(FAdd(fco env x, fco env y))
  | FSub(x, y) -> Ans(FSub(fco env x, fco env y))
  | FMul(x, y) -> Ans(FMul(fco env x, fco env y))
  | FDiv(x, y) -> Ans(FDiv(fco env x, fco env y))
  | FInv(x) -> Ans(FInv(fco env x))
  | FSqrt(x) -> Ans(FSqrt(fco env x))


  | LdR (x, y) when M.mem x env && M.mem y env && -0x8000 <= M.find x env + M.find y env && M.find x env + M.find y env <= 0x7FFF ->
      Ans(LdI (reg_0, M.find x env + M.find y env))
  | FLdR (x, y) when M.mem x env && M.mem y env && -0x8000 <= M.find x env + M.find y env && M.find x env + M.find y env <= 0x7FFF ->
      Ans(FLdI (reg_0, M.find x env + M.find y env))
  | LdI (x, y) when M.mem x env && -0x8000 <= M.find x env + y && M.find x env + y <= 0x7FFF ->
      Ans(LdI(reg_0, M.find x env + y))
  | FLdI (x, y) when M.mem x env && -0x8000 <= M.find x env + y && M.find x env + y <= 0x7FFF ->
      Ans(FLdI(reg_0, M.find x env + y))
  | StI (v, x, y) when M.mem x env && -0x8000 <= M.find x env + y && M.find x env + y <= 0x7FFF ->
      Ans(StI(v, reg_0, M.find x env + y))
  | FStI (v, x, y) when M.mem x env && -0x8000 <= M.find x env + y && M.find x env + y <= 0x7FFF ->
      Ans(FStI(v, reg_0, M.find x env + y))


  | LdR (x, y) when M.mem y env && is16 (M.find y env) ->
      Ans(LdI (co env x, M.find y env))
  | LdR (x, y) when M.mem x env && is16 (M.find x env) ->
      Ans(LdI (co env y, M.find x env))

  | FLdR(x, y) when M.mem y env && is16 (M.find y env) ->
      Ans(FLdI(co env x, M.find y env))
  | FLdR(x, y) when M.mem x env && is16 (M.find x env) ->
      Ans(FLdI(co env y, M.find x env))

  | LdI (x, y) -> Ans(LdI(co env x, y))
  | FLdI (x, y) -> Ans(FLdI(co env x, y))

  | StI(x, y, z) -> Ans(StI(co env x, co env y, z))
  | FStI(x, y, z) -> Ans(FStI(fco env x, co env y, z))

  | IfEq(x, y, e1, e2) when M.mem x env && M.mem y env ->
      if M.find x env = M.find y env then  g env e1 else g env e2
  | IfLE(x, y, e1, e2) when M.mem x env && M.mem y env ->
      if M.find x env <= M.find y env then  g env e1 else g env e2
  | IfLT(x, y, e1, e2) when M.mem x env && M.mem y env ->
      if M.find x env < M.find y env then  g env e1 else g env e2

  | IfEq (x, y, e1, e2) -> Ans(IfEq(co env x, co env y, g env e1, g env e2))
  | IfLE (x, y, e1, e2) -> Ans(IfLE(co env x, co env y, g env e1, g env e2))
  | IfLT (x, y, e1, e2) -> Ans(IfLT(co env x, co env y, g env e1, g env e2))
  | IfFEq(x, y, e1, e2) -> Ans(IfFEq(fco env x, fco env y, g env e1, g env e2))
  | IfFLE(x, y, e1, e2) -> Ans(IfFLE(fco env x, fco env y, g env e1, g env e2))
  | IfFLT(x, y, e1, e2) -> Ans(IfFLT(fco env x, fco env y, g env e1, g env e2))

  | CallDir(Id.L("min_caml_mul"), x::y::[], []) when M.mem x env && is_bin (M.find x env) ->
      Ans(SllI(y, log2 (M.find x env)))
  | CallDir(Id.L("min_caml_mul"), x::y::[], []) when M.mem y env && is_bin (M.find y env) ->
      Ans(SllI(x, log2 (M.find y env)))

  | exp -> Ans(exp)

(* トップレベル関数の16bit即値最適化 *)
let h { name = l; args = xs; fargs = ys; body = e; ret = t } = 
  { name = l; args = xs; fargs = ys; body = g M.empty e; ret = t }

(* プログラム全体の16bit即値最適化 *)
let f (Prog(fundefs, e)) = 
  Prog(List.map h fundefs, g M.empty e)
