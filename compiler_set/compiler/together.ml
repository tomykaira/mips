open Asm

(* いろいろな最適化を行うモジュール。 *)

(* その変数がFNegのみで使われるかどうか判定 *)
let rec fneg_only x = function
  | Ans(exp) -> fneg_only' x exp
  | Let(_, exp, e) -> fneg_only' x exp && fneg_only x e
and fneg_only' x = function
  | FMov(y) | FInv(y) | FSqrt(y) | FMovI(y) | FStI(y,_,_) -> x <> y
  | FAdd(y,z) | FSub(y,z) | FMul(y,z) | FMulN(y,z) | FDiv(y,z) | FDivN(y,z) -> x <> y && x <> z 
  | IfEq(_,_,e1,e2) | IfLT(_,_,e1,e2) | IfLE(_,_,e1,e2)
    -> fneg_only x e1 && fneg_only x e2
  | IfFEq(y,z,e1,e2) | IfFLT(y,z,e1,e2) | IfFLE(y,z,e1,e2)
    -> not (x = y || x = z) && fneg_only x e1 && fneg_only x e2
  | CallCls(_, _, _, zs) | CallDir(_, _, zs) -> not (List.mem x zs)
  | _ -> true

(* FNegをFMovに置き換える *)
let rec rep x = function
  | Ans(exp) -> Ans(rep' x exp)
  | Let(xt, exp, e) -> Let(xt, rep' x exp, rep x e)
and rep' x = function
  | IfEq(y,z,e1,e2) -> IfEq(y,z,rep x e1, rep x e2)
  | IfLE(y,z,e1,e2) -> IfLE(y,z,rep x e1, rep x e2)
  | IfLT(y,z,e1,e2) -> IfLT(y,z,rep x e1, rep x e2)
  | IfFEq(y,z,e1,e2) -> IfFEq(y,z,rep x e1, rep x e2)
  | IfFLE(y,z,e1,e2) -> IfFLE(y,z,rep x e1, rep x e2)
  | IfFLT(y,z,e1,e2) -> IfFLT(y,z,rep x e1, rep x e2)
  | FNeg(y) when x = y -> FMov(x)
  | exp -> exp

let find x env = try M.find x env with Not_found -> Nop
(* 複数の命令を一つにまとめる関数 *)
let rec g env = function
  | Ans(exp) -> Ans(g' env exp)
  | Let((x, _) as xt, exp, e) ->
      let exp' = g' env exp in
      if is_reg x then Let(xt, exp', g env e) else
      let (b, exp'', e') =
	(match exp' with
	| FMul(y,z) when fneg_only x e ->
	    (false, FMulN(y,z), g env (rep x e))
	| FMulN(y,z) when fneg_only x e ->
	    (false, FMul(y,z), g env (rep x e))
	| FDiv(y,z) when fneg_only x e ->
	    (false, FDivN(y,z), g env (rep x e))
	| FDivN(y,z) when fneg_only x e ->
	    (false, FDiv(y,z), g env (rep x e))
        | FNeg(y) | AddI(y, _) | SubI(y, _) when not (is_reg y) ->
            (true, exp', g (M.add x exp' env) e)
	| Sub(z, y) when z = reg_f0 && not (is_reg y) ->
	    (true, exp', g (M.add x (FNeg(y)) env) e)
	| FMov(y) when not (is_reg y) && M.mem y env ->
	    (true, exp', g (M.add x (M.find y env) env) e)
	| _ -> (false, exp', g env e)) in
      if b && not (List.mem x (fv e')) then e' else Let(xt, exp'', e')
and g' env = function
  | AddI(x, i) as exp ->
      (match find x env with
      | AddI(y, j) when is16 (i+j) -> AddI(y, i+j)
      | SubI(y, j) when is16 (i-j) -> AddI(y, i-j)
      | _ -> exp)
  | SubI(x, i) as exp ->
      (match find x env with
      | AddI(y, j) when is16 (-i+j) -> AddI(y, -i+j)
      | SubI(y, j) when is16 (-i-j) -> AddI(y, -i-j)
      | _ -> exp)

  | FMul(x, y) as exp ->
      (match (find x env, find y env) with
      | (FNeg(z), FNeg(w)) -> FMul(z, w)
      | (FNeg(z), _) -> FMulN(z, y)
      | (_, FNeg(w)) -> FMulN(x, w)
      | _ -> exp)
  | FDiv(x, y) as exp ->
      (match (find x env, find y env) with
      | (FNeg(z), FNeg(w)) -> FDiv(z, w)
      | (FNeg(z), _) -> FDivN(z, y)
      | (_, FNeg(w)) -> FDivN(x, w)
      | _ -> exp)
  | FNeg(x) as exp ->
      (match find x env with
      | FNeg(y) -> FMov(y)
      | _ -> exp)

  | LdI(x, i) as exp ->
      (match find x env with
      | AddI(y, j) when is16 (i+j) -> LdI(y, i+j)
      | SubI(y, j) when is16 (i-j) -> LdI(y, i-j)
      | _ -> exp)
  | StI(z, x, i) as exp ->
      (match find x env with
      | AddI(y, j) when is16 (i+j) -> StI(z, y, i+j)
      | SubI(y, j) when is16 (i-j) -> StI(z, y, i-j)
      | _ -> exp)
  | FLdI(x, i) as exp ->
      (match find x env with
      | AddI(y, j) when is16 (i+j) -> FLdI(y, i+j)
      | SubI(y, j) when is16 (i-j) -> FLdI(y, i-j)
      | _ -> exp)
  | FStI(z, x, i) as exp ->
      (match find x env with
      | AddI(y, j) when is16 (i+j) -> FStI(z, y, i+j)
      | SubI(y, j) when is16 (i-j) -> FStI(z, y, i-j)
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
