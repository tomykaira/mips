open Asm

(* Neg + Mul -> FMulNの最適化を行うモジュール。 *)

let find x env = try M.find x env with Not_found -> Nop
(* 複数の命令を一つにまとめる関数 *)
let rec g env = function
  | Ans(exp) -> Ans(g' env exp)
  | Let((x, _) as xt, exp, e) ->
      let exp' = g' env exp in
      (match exp' with
      | FNeg(y) as fneg when not (is_reg x || is_reg y) ->
          let e' = g (M.add x fneg env) e in
          if List.mem x (fv e') then Let(xt, exp', e') else e'
      | FMov(y) when not (is_reg x || is_reg y) && M.mem y env ->
	  let e' = g (M.add x (M.find y env) env) e in
          if List.mem x (fv e') then Let(xt, exp', e') else e'
      | _ -> Let(xt, exp', g env e))
and g' env = function
  | FMul(x, y) as exp ->
      (match (find x env, find y env) with
      | (FNeg(z), FNeg(w)) -> FMul(z, w)
      | (FNeg(z), _) -> FMulN(z, y)
      | (_, FNeg(z)) -> FMulN(x, z)
      | _ -> exp)
  | FDiv(x, y) as exp ->
      (match (find x env, find y env) with
      | (FNeg(z), FNeg(w)) -> FDiv(z, w)
      | (FNeg(z), _) -> FDivN(z, y)
      | (_, FNeg(z)) -> FDivN(x, z)
      | _ -> exp)
  | FNeg(x) as exp ->
      (match find x env with
      | FNeg(y) -> FMov(y)
      | _ -> exp)

  | IfEq(x, y, e1, e2) -> IfEq(x, y, g env e1, g env e2)
  | IfLE(x, y, e1, e2) -> IfLE(x, y, g env e1, g env e2)
  | IfLT(x, y, e1, e2) -> IfLT(x, y, g env e1, g env e2)
  | IfFEq(x, y, e1, e2) -> IfFEq(x, y, g env e1, g env e2)
  | IfFLE(x, y, e1, e2) -> IfFLE(x, y, g env e1, g env e2)
  | IfFLT(x, y, e1, e2) -> IfFLT(x, y, g env e1, g env e2)

  | exp -> exp

let f (Prog(toplevel, e)) =
  Format.eprintf "gathering instructions...@.";
  Prog(List.map (fun { name = l; args = ys; fargs = zs; body = e; ret = t } ->
    { name = l; args = ys; fargs = zs; body = g M.empty e; ret = t }) toplevel,
    g M.empty e)
