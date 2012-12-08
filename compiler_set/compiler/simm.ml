open Asm

(* 16bitの即値最適化と定数畳み込みを行うモジュール *)

let memi x env =
  try (match M.find x env with Int _ -> true | _ -> false)
  with Not_found -> false
let memf x env =
  try (match M.find x env with Float _ -> true | _ -> false)
  with Not_found -> false

let findi x env = (match M.find x env with Int(i) -> i | _ -> raise Not_found)
let findf x env = (match M.find x env with Float(d) -> d | _ -> raise Not_found)


let co env x = if memi x env && findi x env = 0 then reg_0 
               else if memi x env && findi x env = 1 then reg_1 
               else if memi x env && findi x env = -1 then reg_m1 else x

let fco env x = if memf x env && findf x env = 0.0 then reg_f0 else x




(* その数が2の何乗か返す *)
let rec log2_sub n i =
  if 1 lsl (i+1) > n then i
  else log2_sub n (i+1)
let log2 n = log2_sub n 0
(* その数が2のべき乗か返す *)
let is_bin n = 1 lsl (log2 n) = n

let is16 i = -0x8000 <= i && i <= 0x7FFF

let rec g env = function (* 命令列の16bit即値最適化 *)
  | Ans(exp) -> Ans(g' env exp)    
  | Let((x, _) as xt, exp, e) ->
      let exp' = g' env exp in
      if is_reg x then Let(xt, exp', g env e) else
      let (b, env') = match exp' with
      | Int(_) -> (true, M.add x exp' env)
      | Float(_) -> (true, M.add x exp' env)
      | _ -> (false, env) in
      let e' = g env' e in
      if b && not (List.mem x (fv e')) then e'
      else Let(xt, exp', e')
and g' env = function (* 各命令の16bit即値最適化 *)
  | Add(x, y) when memi x env && memi y env ->
      Int(findi x env + findi y env)
  | Sub(x, y) when memi x env && memi y env ->
      Int(findi x env - findi y env)
  | Xor(x, y) when memi x env && memi y env ->
      Int((findi x env) lxor (findi y env))

  | AddI(x, i) when memi x env -> Int(findi x env + i)
  | AddI(x, i) when x = reg_0 -> Int(i)
  | SubI(x, i) when memi x env -> Int(findi x env - i)
  | XorI(x, i) when memi x env -> Int((findi x env) lxor i)

  | SllI(x, i) when memi x env && Int32.shift_left (Int32.of_int (findi x env)) i = Int32.of_int ((findi x env) lsl i) ->
      Int((findi x env) lsl i)
  | SraI(x, i) when memi x env ->
      Int((findi x env) asr i)


  | Add(x, y) when memi y env && is16 (findi y env) ->
      AddI(x, findi y env)
  | Add(x, y) when memi x env && is16 (findi x env) ->
      AddI(y, findi x env)
  | Sub(x, y) when memi y env && is16 (findi y env) ->
      SubI(x, findi y env)
  | Xor(x, y) when memi y env && is16 (findi y env) ->
      XorI(x, findi y env)
  | Xor(x, y) when memi x env && is16 (findi x env) ->
      XorI(y, findi x env)

  | FMov(x) -> FMov(fco env x)
  | FNeg(x) -> FNeg(fco env x)
  | FAdd(x, y) -> FAdd(fco env x, fco env y)
  | FSub(x, y) -> FSub(fco env x, fco env y)
  | FMul(x, y) -> FMul(fco env x, fco env y)
  | FDiv(x, y) -> FDiv(fco env x, fco env y)
  | FInv(x) -> FInv(fco env x)
  | FSqrt(x) -> FSqrt(fco env x)


  | LdR (x, y) when memi x env && memi y env && is16 (findi x env + findi y env) ->
      LdI (reg_0, findi x env + findi y env)
  | FLdR (x, y) when memi x env && memi y env && is16 (findi x env + findi y env) ->
      FLdI (reg_0, findi x env + findi y env)
  | LdI (x, y) when memi x env && is16 (findi x env + y) ->
      LdI(reg_0, findi x env + y)
  | FLdI (x, y) when memi x env && is16 (findi x env + y) ->
      FLdI(reg_0, findi x env + y)
  | StI (v, x, y) when memi x env && is16 (findi x env + y) ->
      StI(co env v, reg_0, findi x env + y)
  | FStI (v, x, y) when memi x env && is16 (findi x env + y) ->
      FStI(fco env v, reg_0, findi x env + y)



  | LdR (x, y) when memi y env && is16 (findi y env) ->
      LdI (co env x, findi y env)
  | LdR (x, y) when memi x env && is16 (findi x env) ->
      LdI (co env y, findi x env)

  | FLdR(x, y) when memi y env && is16 (findi y env) ->
      FLdI(co env x, findi y env)
  | FLdR(x, y) when memi x env && is16 (findi x env) ->
      FLdI(co env y, findi x env)

  | LdI (x, y) -> LdI(co env x, y)
  | FLdI (x, y) -> FLdI(co env x, y)

  | StI(x, y, z) -> StI(co env x, co env y, z)
  | FStI(x, y, z) -> FStI(fco env x, co env y, z)

  | IfEq (x, y, e1, e2) -> IfEq(co env x, co env y, g env e1, g env e2)
  | IfLE (x, y, e1, e2) -> IfLE(co env x, co env y, g env e1, g env e2)
  | IfLT (x, y, e1, e2) -> IfLT(co env x, co env y, g env e1, g env e2)
  | IfFEq(x, y, e1, e2) -> IfFEq(fco env x, fco env y, g env e1, g env e2)
  | IfFLE(x, y, e1, e2) -> IfFLE(fco env x, fco env y, g env e1, g env e2)
  | IfFLT(x, y, e1, e2) -> IfFLT(fco env x, fco env y, g env e1, g env e2)

  | CallDir(Id.L("min_caml_mul"), x::y::[], []) when memi x env && is_bin (findi x env) ->
      SllI(y, log2 (findi x env))
  | CallDir(Id.L("min_caml_mul"), x::y::[], []) when memi y env && is_bin (findi y env) ->
      SllI(x, log2 (findi y env))

  | exp -> exp

(* トップレベル関数の16bit即値最適化 *)
let h { name = l; args = xs; fargs = ys; body = e; ret = t } = 
  { name = l; args = xs; fargs = ys; body = g M.empty e; ret = t }

(* プログラム全体の16bit即値最適化 *)
let f (Prog(fundefs, e)) = 
  Prog(List.map h fundefs, g M.empty e)
