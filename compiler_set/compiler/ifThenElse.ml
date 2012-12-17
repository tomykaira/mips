open ANormal

(* if文に関する最適化 *)

let dummy = (Id.genid "dummy", Type.Unit)

(* 以降の自由変数にxやfvsに含まれない変数が含まれないような地点を探す関数 *)
let rec h x fvs n = function
  | Let(xt, exp, e) ->
      let n' = n - size' exp in
      if n' < 0 then raise Not_found else
      let fvs' = fv e in
      if not (S.mem x fvs') && S.for_all (fun x -> S.mem x fvs) fvs' then
	(Ans(exp), xt, e)
      else let (m, yt, e') = h x fvs n' e in (Let(xt,exp,m), yt, e')
  | LetRec({ name = xt; args = yts; body = e1 }, e2) ->
      let (m, yt, e2') = h x fvs (n-1) e2 in
      (LetRec({ name = xt; args = yts; body = e1 }, m), yt, e2')
  | LetTuple(xts, y ,e) ->
      let (m, yt, e') = h x fvs (n-1) e in
      (LetTuple(xts, y, m), yt, e')
  | LetList(xt, y, e) ->
      let (m, yt, e') = h x fvs (n-1) e in
      (LetList(xt, y, m), yt, e')
  | Ans(exp) ->
      if n < 0 then raise Not_found else
      (Ans(exp), dummy, Ans(Unit))


(* 即値の葉があるか調べる関数 *)
let rec immans  = function
  | Let(_,_,e) | LetRec(_,e) | LetTuple(_,_,e) | LetList(_,_,e) -> immans e
  | Ans(exp) -> immans' exp
and immans' = function
  | Int _ | Float _ -> true
  | IfEq(_,_,e1,e2) | IfLE(_,_,e1,e2) | IfLT(_,_,e1,e2) | IfNil(_,e1,e2) ->
      immans e1 || immans e2
  | _ -> false
(* 葉が全て即値か調べる関数 *)
(*let rec allimm  = function
  | Let(_,_,e) | LetRec(_,e) | LetTuple(_,_,e) | LetList(_,_,e) -> allimm e
  | Ans(exp) -> allimm' exp
and allimm' = function
  | Int _ | Float _ -> true
  | IfEq(_,_,e1,e2) | IfLE(_,_,e1,e2) | IfLT(_,_,e1,e2) | IfNil(_,e1,e2) ->
      allimm e1 && allimm e2
  | _ -> false *)

let tailsize = 10

(* 本体 *)
let rec g tail = function
  | Let((x,t) as xt, exp, e) ->
      let len =
	if size e < tailsize then 1000000 else
	if immans' exp then 6
	else 0 in
      (match exp with
      | IfEq(p,q,e1,e2) when len > 0 ->
	  (try let (m, yt, e') =
	    (*if false && tail && size e < tailsize then (e,dummy, Ans(Unit))
	    else*) h x (fv e) len e in
	  let z = Id.genid x in
	  let exp' = IfEq(p,q,concat e1 xt m, concat e2 (z,t) (ag (M.singleton x z) m)) in
	  if e' = Ans(Unit) then Ans(g' tail exp') else
	  g tail (Let(yt, exp', e'))
	  with Not_found -> Let(xt, g' false exp, g tail e))
      | IfLE(p,q,e1,e2) when len > 0 ->
	  (try let (m, yt, e') =
	    (*if tail && size e < tailsize then (e,dummy,Ans(Unit))
	    else*) h x (fv e) len e in
	  let z = Id.genid x in
	  let exp' = IfLE(p,q,concat e1 xt m, concat e2 (z,t) (ag (M.singleton x z) m)) in
	  if e' = Ans(Unit) then Ans(g' tail exp') else
	  g tail (Let(yt, exp', e'))
	  with Not_found -> Let(xt, g' false exp, g tail e)) 
      | IfLT(p,q,e1,e2) when len > 0 ->
	  (try let (m, yt, e') =
	    (*if false && tail && size e < tailsize then (e,dummy,Ans(Unit))
	    else*) h x (fv e) len e in
	  let z = Id.genid x in
	  let exp' = IfLT(p,q,concat e1 xt m, concat e2 (z,t) (ag (M.singleton x z) m)) in 
	  if e' = Ans(Unit) then Ans(g' tail exp') else
	  g tail (Let(yt, exp', e'))
	  with Not_found -> Let(xt, g' false exp, g tail e))
      | IfNil(p,e1,e2) when len > 0 ->
	  (try let (m, yt, e') =
	    (*if tail && size e < tailsize then (e,dummy,Ans(Unit))
	    else*) h x (fv e) len e in
	  let z = Id.genid x in
	  let exp' = IfNil(p,concat e1 xt m, concat e2 (z,t) (ag (M.singleton x z) m)) in
	  if e' = Ans(Unit) then Ans(g' tail exp') else
	  g tail (Let(yt, exp', e'))
	  with Not_found -> Let(xt, g' false exp, g tail e))
      | _ -> Let(xt, g' false exp, g tail e))
  | LetRec({ name = xt; args = yts; body = e1 }, e2) ->
      LetRec({ name = xt; args = yts; body = g true e1 }, g tail e2)
  | LetTuple(xts, y, e) -> LetTuple(xts, y, g tail e)
  | LetList(xt, y, e) -> LetList(xt, y, g tail e)
  | Ans(exp) -> Ans(g' tail exp)
and g' tail = function
  | IfEq(x, y, e1, e2) -> IfEq(x, y, g tail e1, g tail e2)
  | IfLE(x, y, e1, e2) -> IfLE(x, y, g tail e1, g tail e2)
  | IfLT(x, y, e1, e2) -> IfLT(x, y, g tail e1, g tail e2)
  | IfNil(x, e1, e2) -> IfNil(x, g tail e1, g tail e2)
  | exp -> exp


let f e =
  Format.eprintf "if then else...@.";
  g true e
