open Closure

(* ネストしたletの簡約 *)

let rec g = function (* ネストしたletの簡約 *)
  | IfEq(x, y, e1, e2) -> IfEq(x, y, g e1, g e2)
  | IfLE(x, y, e1, e2) -> IfLE(x, y, g e1, g e2)
  | IfLT(x, y, e1, e2) -> IfLT(x, y, g e1, g e2)
  | Let(xt, e1, e2) -> (* letの場合 *)
      let rec insert = function
	| Let(yt, e3, e4) -> Let(yt, e3, insert e4)
	| MakeCls(xt, cl, e) -> MakeCls(xt, cl, insert e)
	| LetTuple(yts, z, e) -> LetTuple(yts, z, insert e)
	| e -> Let(xt, e, g e2) in
      insert (g e1)
  | MakeCls(xt, cl, e2) ->
      MakeCls(xt, cl, g e2)
  | LetTuple(xts, y, e) -> LetTuple(xts, y, g e)
  | e -> e

let f (Prog(toplevel, e)) =
  Prog(List.map (fun { name = xt; args = yts; formal_fv = zts; body = e } ->
    { name = xt; args = yts; formal_fv = zts; body = g e }) toplevel,
       g e)
