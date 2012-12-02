open Asm

(* いろいろな最適化を行うモジュール。 *)

let find x env = try M.find x env with Not_found -> Nop
(* 複数の命令を一つにまとめる関数 *)
let rec g env = function
  | Ans(exp) -> Ans(g' env exp)
  | Let((x, _) as xt, exp, e) ->
      let exp' = g' env exp in
      if is_reg x then Let(xt, exp', g env e) else
      let (b, e') =
	(match exp' with
        | FNeg(y) | AddI(y, _) | SubI(y, _) when not (is_reg y) ->
            (true, g (M.add x exp' env) e)
	| FMov(y) when not (is_reg y) && M.mem y env ->
	    (true, g (M.add x (M.find y env) env) e)
	| _ -> (false, g env e)) in
      if b && not (List.mem x (fv e')) then e' else Let(xt, exp', e')
and g' env = function
  | FMul(x, y) as exp ->
      (match (find x env, find y env) with
      | (FNeg(z), FNeg(w)) -> FMul(z, w)
      | _ -> exp)
  | FDiv(x, y) as exp ->
      (match (find x env, find y env) with
      | (FNeg(z), FNeg(w)) -> FDiv(z, w)
      | _ -> exp)
  | FNeg(x) as exp ->
      (match find x env with
      | FNeg(y) -> FMov(y)
      | _ -> exp)

  | LdI(x, i) as exp ->
      (match find x env with
      | AddI(y, j) when -0x8000 <= i+j && i+j <= 0x7FFF -> LdI(y, i+j)
      | SubI(y, j) when -0x8000 <= i-j && i-j <= 0x7FFF -> LdI(y, i-j)
      | _ -> exp)
  | StI(z, x, i) as exp ->
      (match find x env with
      | AddI(y, j) when -0x8000 <= i+j && i+j <= 0x7FFF -> StI(z, y, i+j)
      | SubI(y, j) when -0x8000 <= i-j && i-j <= 0x7FFF -> StI(z, y, i-j)
      | _ -> exp)
  | FLdI(x, i) as exp ->
      (match find x env with
      | AddI(y, j) when -0x8000 <= i+j && i+j <= 0x7FFF -> FLdI(y, i+j)
      | SubI(y, j) when -0x8000 <= i-j && i-j <= 0x7FFF -> FLdI(y, i-j)
      | _ -> exp)
  | FStI(z, x, i) as exp ->
      (match find x env with
      | AddI(y, j) when -0x8000 <= i+j && i+j <= 0x7FFF -> FStI(z, y, i+j)
      | SubI(y, j) when -0x8000 <= i-j && i-j <= 0x7FFF -> FStI(z, y, i-j)
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
