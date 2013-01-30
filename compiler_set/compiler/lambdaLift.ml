(*pp deriving *)

open ANormal


(* 引数を渡すのに使えるレジスタの数 *)
let rn = 26
let fn = 30

(* クロージャ変換でグローバル配列,クロージャになる変数の集合 *)
let globals = ref []

(* 変数の集合からそれぞれrn,fn個をとってリストにして返す関数 *)
let rec take l known env rn fn fvs =
  if S.is_empty fvs then l
  else let x = S.choose fvs in
       let fvs' = S.remove x fvs in
       match M.find x env with
       | Type.Unit -> take l known env rn fn fvs'
       | Type.Float as t ->
	   if List.mem_assoc x l then take l known env rn fn fvs'
	   else if fn <= 0 then failwith "float"
           else take ((x,t)::l) known env rn (fn-1) fvs'
       | t -> if List.mem_assoc x l || List.mem x !globals || S.mem x known then
	        take l known env rn fn fvs'
	      else if rn <= 0 then failwith "non float"
              else take ((x,t)::l) known env (rn-1) fn fvs'


(* 関数に引数を追加する関数 *)
let rec h s add = function
  | Let((x, t), exp, e) -> Let((x, t), h' s add exp, h s add e)
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) ->
      LetRec({ name = (x, t); args = yts; body = h s add e1 }, h s add e2)
  | LetTuple (xts, y, e) -> LetTuple(xts, y, h s add e)
  | LetList(xs, y, e) -> LetList(xs, y, h s add e)
  | Ans(exp) -> Ans(h' s add exp)
and h' s add = function
  | IfEq(x, y, e1, e2) -> IfEq(x, y, h s add e1, h s add e2)
  | IfLE(x, y, e1, e2) -> IfLE(x, y, h s add e1, h s add e2)
  | IfLT(x, y, e1, e2) -> IfLT(x, y, h s add e1, h s add e2)
  | IfNil(x, e1, e2) -> IfNil(x, h s add e1, h s add e2)
  | App(y, l) when y = s -> App(y, l@add)
  | exp -> exp

(* 式の中に関数が関数適用以外の形で現れてないか調べる関数 *)
let rec app_only x = function
  | Let((_, _), exp, e) -> app_only' x exp && app_only x e
  | LetRec({ name = _; args = _; body = e1 }, e2) ->
      app_only x e1 && app_only x e2
  | LetTuple (_, _, e) | LetList(_, _, e) ->  app_only x e
  | Ans(exp) -> app_only' x exp
and app_only' x = function
  | IfEq(_, _, e1, e2) | IfLE(_, _, e1, e2) | IfLT(_, _, e1, e2) | IfNil(_, e1, e2) ->
      app_only x e1 && app_only x e2
  | App(_, l) | ExtFunApp(_, l) | Tuple(l) -> not (List.mem x l)
  | Var(y) | Put(_,_,y) | Cons(y, _) -> x <> y 
  | _ -> true

(* 本体 *)
let rec g env known top = function
  | Let((x, t), exp, e) ->
      (match t with
      | Type.Array _ when top -> globals := x::!globals
      | _ -> ());
      Let((x, t), g' env known exp, g (M.add x t env) known top e)
  | LetRec({ name = (x, (Type.Fun(p,q) as t)); args = yts; body = e1 }, e2) ->
      (if top then globals := x::!globals);
      let env' = M.add x t env in
      let env'' = M.add_list yts env' in
      (try
        let fvs = S.diff (fv e1) (S.of_list (x::List.map fst yts)) in
        let yts' =
	  take [] known env'
	    (rn - List.length (List.filter (fun yt -> snd yt <> Type.Float && snd yt <> Type.Unit) yts))
	    (fn - List.length (List.filter (fun yt -> snd yt = Type.Float) yts))
  	    fvs in
	if yts' = [] then
	  let known' = S.add x known in
	  LetRec({ name = (x, t); args = yts; body = g env'' known' false e1 }, g env' known' top e2)
	else
          let yts'' = List.map (fun (y, t) -> (Id.genid y, t)) yts' in
          let benv = List.fold_left2 (fun x y z -> M.add y z x)
	               M.empty (List.map fst yts') (List.map fst yts'') in
	  let t' = Type.Fun(p@(List.map snd yts'), q) in
	  let env3 = M.add x t' env in
          LetRec({ name = (x, t'); args = yts@yts''; body = g (M.add_list (yts@yts'') env3) (S.add x known) false (h x (List.map fst yts'') (Beta.g benv e1)); },
	         g env3 (S.add x known) top (h x (List.map fst yts') e2))
	with Failure _ ->
	  LetRec({ name = (x, t); args = yts; body = g env'' known false e1 }, g env' known top e2))
  | LetTuple (xts, y, e) -> LetTuple(xts, y, g (M.add_list xts env) known top e)
  | LetList ((xs,t), y, e) ->
      LetList((xs,t), y, g (M.add_list (List.map (fun z -> (z,t)) (Syntax.matcher_variables xs)) env) known top e)
  | Ans(exp) -> Ans(g' env known exp)
  | _ -> assert false
and g' env known = function
  | IfEq(x, y, e1, e2) -> IfEq(x, y, g env known false e1, g env known false e2)
  | IfLE(x, y, e1, e2) -> IfLE(x, y, g env known false e1, g env known false e2)
  | IfLT(x, y, e1, e2) -> IfLT(x, y, g env known false e1, g env known false e2)
  | IfNil(x, e1, e2) -> IfNil(x, g env known false e1, g env known false e2)
  | e -> e


let f e = Format.eprintf "lambda lifting...@.";
          g M.empty S.empty true e
