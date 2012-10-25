(* 関数の自由変数をできるだけ引数渡しにし,クロージャを減らすモジュール *)

open KNormal

(* スコアと変数のペアの順序付き集合 *)
module S' =
  Set.Make
    (struct
      type t = int * string
      let compare = compare
    end)


(* 引数を渡すのに使えるレジスタの数 *)
let rn = 25
let fn = 31


(* 変数にスコアづけする関数。後に出てくるほど高得点 *)
let rec score en n fvs = function
  | IfEq(x, y, e1, e2) | IfLE(x, y, e1, e2)  | IfLT(x, y, e1, e2) -> 
      let r = S'.union (score en (n+1) fvs e1) (score en (n+1) fvs e2) in
      let r = if S.mem x fvs then S'.add (n,x) r else r in
      if S.mem y fvs then S'.add (n,y) r else r  
  | Let((_, _), e1, e2) ->
      S'.union (score en (n+1) fvs e1) (score en (n+1+Inline.size e1) fvs e2) 
  | LetRec({ name = (x, _); args = _; body = e1 }, e2) ->
      score (M.add x (S.inter (fv e1) fvs) en) (n+1) fvs e2 
  | App(x, t) ->
      let r = List.fold_left (fun y x -> if S.mem x fvs then S'.add (n,x) y else y) S'.empty t in
      if M.mem x en then S.fold (fun x y-> S'.add (n, x) y) (M.find x en) r
      else r
  | LetTuple(_, x, e) ->
      let r = score en (n+1) fvs e in
      if S.mem x fvs then S'.add (n,x) r else r
  | e -> S.fold (fun x y -> S'.add (n, x) y) (S.inter (fv e) fvs) S'.empty


(* スコアづけされた変数の集合から上位rn,fn個をとってリストにして返す関数 *)
let rec take l tenv rn fn fvs =
  if (rn <= 0 && fn <= 0) || S'.is_empty fvs then l
  else let (n, x) = S'.max_elt fvs in
       let fvs' = S'.remove (n, x) fvs in
       match M.find x tenv with
       | Type.Unit | Type.Fun(_,_) -> take l tenv rn fn fvs' 
       | Type.Float -> if List.mem_assoc x l then
	                 take l tenv rn fn fvs'
	               else if fn <= 0 then raise Not_found
                       else take ((x,Type.Float)::l) tenv rn (fn-1) fvs'
       | t -> if List.mem_assoc x l then take l tenv rn fn fvs'
	      else if rn <= 0 then raise Not_found
              else take ((x,t)::l) tenv (rn-1) fn fvs'


(* 関数に引数を追加する関数 *)
let rec h s add = function
  | IfEq(x, y, e1, e2) -> IfEq(x, y, h s add e1, h s add e2)
  | IfLE(x, y, e1, e2) -> IfLE(x, y, h s add e1, h s add e2)
  | IfLT(x, y, e1, e2) -> IfLT(x, y, h s add e1, h s add e2)
  | Let((x, t), e1, e2) -> Let((x, t), h s add e1, h s add e2)
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) ->
      LetRec({ name = (x, t); args = yts; body = h s add e1 }, h s add e2)
  | App(y, l) when y = s -> App(y, l@add)
  | LetTuple (l, y, e) -> LetTuple(l, y, h s add e)
  | e -> e

(* 式の中に関数が関数適用以外の形で現れてないか調べる関数 *)
let rec app_only x = function
  | IfEq(_, _, e1, e2) | IfLE(_, _, e1, e2) | IfLT(_, _, e1, e2)
  | Let((_, _), e1, e2) | LetRec({ name = (_, _); args = _; body = e1 }, e2) ->
      app_only x e1 && app_only x e2
  | App(_, l) | ExtFunApp(_, l) | Tuple(l) -> not (List.mem x l)
  | LetTuple (_, y, e) ->  x <> y && app_only x e
  | Var(y) | Put(_,_,y) -> x <> y 
  | _ -> true

(* 本体 *)
let rec g tenv = function
  | IfEq(x, y, e1, e2) -> IfEq(x, y, g tenv e1, g tenv e2)
  | IfLE(x, y, e1, e2) -> IfLE(x, y, g tenv e1, g tenv e2)
  | IfLT(x, y, e1, e2) -> IfLT(x, y, g tenv e1, g tenv e2)
  | Let((x, t), e1, e2) ->
      let tenv' = M.add x t tenv in
      Let((x, t), g tenv e1, g tenv' e2)
  | LetRec({ name = (x, (Type.Fun(p,q) as t)); args = yts; body = e1 }, e2) ->
      let tenv' = M.add x t tenv in
      if not (app_only x e1 && app_only x e2) then
        LetRec({ name = (x, t); args = yts; body = g (M.add_list yts tenv') e1 },
	         g tenv' e2)
      else
        let fvs = S.diff (fv e1) (S.of_list (x::List.map fst yts)) in
        let fvs' =
	  S.fold (fun x y-> S'.add (0, x) y) fvs (score M.empty 1 fvs e2) in
        let yts' =
	  try
	  take [] tenv'
	    (rn - List.length (List.filter (fun yt -> snd yt <> Type.Float && snd yt <> Type.Unit) yts))
	    (fn - List.length (List.filter (fun yt -> snd yt = Type.Float) yts))
	    fvs' 
	with Not_found -> [] in
        if yts' = []  then
	  LetRec({ name = (x, t); args = yts; body = g (M.add_list yts tenv') e1 }, g tenv' e2)
        else
          let yts'' = List.map (fun (y, t) -> (Id.genid y, t)) yts' in
          let benv = List.fold_left2 (fun x y z -> M.add y z x)
	               M.empty (List.map fst yts') (List.map fst yts'') in
	  let t' = Type.Fun(p@(List.map snd yts'), q) in
	  let tenv'' = M.add x t' tenv in

          LetRec({ name = (x, t'); args = yts@yts''; body = g (M.add_list (yts@yts'') tenv'') (h x (List.map fst yts'') (Beta.g benv e1)) },
	         g tenv'' (h x (List.map fst yts') e2))
  | LetTuple (l, y, e) -> LetTuple(l, y, g (M.add_list l tenv) e)
  | e -> e


let f e = Format.eprintf "Add free variables to argument@.";
          g M.empty e
