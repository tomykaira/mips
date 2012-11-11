open ANormal

(* 各コードをできるだけ後ろに下げるモジュール *)


(* これより前にxsのどれかがが使われるところが無く,fvsのどれかの生存時間を
   延ばすことが無いような一番遅い位置にinstをはさむ関数 *)
let rec h xs fvs inst le =
  if not (S.for_all (fun x -> S.mem x (fv le)) fvs) then
    inst le else
  match le with 
  | Let(xt, exp, e) ->
      if List.exists (fun x -> S.mem x (fv' exp)) xs then
	if List.exists (fun x -> S.mem x (fv e)) xs then inst le
	else concat (h' xs fvs inst exp) xt e
      else Let(xt, exp, h xs fvs inst e)
  | LetRec ({ name = (x, t); args = yts; body = e1 }, e2) ->
      if List.exists (fun x -> S.mem x (fv e1)) xs then
	if List.exists (fun x -> S.mem x (fv e2)) xs then inst le
	else LetRec ({ name = (x, t); args = yts; body = h xs fvs inst e1 }, e2)
      else LetRec ({ name = (x, t); args = yts; body = e1 }, h xs fvs inst e2)
  | LetTuple(xts, y, e) ->
      if List.exists (fun x -> x = y) xs then inst le	
      else LetTuple(xts, y, h xs fvs inst e)
  | Ans(exp) -> h' xs fvs inst exp
and h' xs fvs inst = function
  | IfEq(x, y, e1, e2) as e->
      if List.mem x xs || List.mem y xs then inst (Ans(e)) else
      Ans(IfEq(x, y, h xs fvs inst e1, h xs fvs inst e2))
  | IfLE(x, y, e1, e2) as e->
      if List.mem x xs || List.mem y xs then inst (Ans(e)) else
      Ans(IfLE(x, y, h xs fvs inst e1, h xs fvs inst e2))
  | IfLT(x, y, e1, e2) as e->
      if List.mem x xs || List.mem y xs then inst (Ans(e)) else
      Ans(IfLT(x, y, h xs fvs inst e1, h xs fvs inst e2))
  | exp -> inst (Ans(exp))


(* 本体 *)
let rec g = function
  | Let ((x, t) as xt, exp, e) ->
      let exp' = g' exp in
      let e' = g e in
      let fvs = fv' exp in
      if Elim.effect' S.empty exp || not (S.for_all (fun x -> S.mem x (fv e)) fvs) then Let(xt, exp', e')
      else h [x] fvs (fun s -> Let(xt, exp', s)) e
  | LetRec ({ name = (x, t) as xt; args = yts; body = e1 }, e2) ->
      h [x] (fv e1) (fun s -> LetRec({name=xt; args=yts; body=g e1}, s)) (g e2)
  | LetTuple(xts, y, e) ->
      h (List.map fst xts) (S.empty) (fun s -> LetTuple(xts, y, s)) (g e)
  | Ans(exp) -> Ans(g' exp)
and g' = function
  | IfEq(x, y, e1, e2) -> IfEq(x, y, g e1, g e2)
  | IfLE(x, y, e1, e2) -> IfLE(x, y, g e1, g e2)
  | IfLT(x, y, e1, e2) -> IfLT(x, y, g e1, g e2)
  | exp -> exp


let f e =
  Format.eprintf "Lazy code motion...@.";
  g e
