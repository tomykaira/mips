open Closure
(* 関数の引数のタプルを展開するモジュール *)

(* スコアと変数のペアの順序付き集合 *)
module S' =
  Set.Make
    (struct
      type t = (int * int) * (string * Type.t list)
      let compare = compare
    end)


(* 引数を渡すのに使えるレジスタの数 *)
let rn = 25
let fn = 31


(* 型のリストの中に整数レジスタ,小数レジスタに割り当てられる変数が
   いくつあるか調べる関数 *)
let count ts =
  let int = function
      | Type.Unit | Type.Float -> false
      | _ -> true in
  let float x = x = Type.Float in
  (List.length (List.filter int ts), List.length (List.filter float ts)) 


(* 引数の数がレジスタ数を超えないように引数のタプルを展開する関数 *)
let rec h' tups r yts =
  let yts' = List.concat yts in
  if S'.is_empty tups then (yts', r, List.map (List.map snd) yts)
  else let (n, (x, l)) = S'.min_elt tups in
       let (int , float)  = count (List.map snd yts') in
       let (int', float') = count l in
       let tups' = S'.remove (n,(x,l)) tups in
       if int+int'-1 > rn || float+float' > fn then h' tups' r yts
       else
	 let wts = List.map (fun t -> (Id.genid x, t)) l in
         let unf = function
                   | [(y, _)] when y = x -> wts
                   | y -> y in
         h' tups' (((x, Type.Tuple(l)), wts)::r) (List.map unf yts)

let h_args yts =
  let add (x,n) (y, t) = match t with
    | Type.Tuple(l) -> (S'.add ((List.length l, n), (y, l)) x, n+1)
    | _ -> (x, n) in
  let (tups, _) = List.fold_left add (S'.empty,0) yts in
  h' tups [] (List.map (fun x -> [x]) yts)


(* 自由変数のタプルを展開する関数 *)
let h'' zts =
  let r = List.fold_left
            (fun l (x, t) -> match t with
	                     | Type.Tuple(ts) -> ((x, Type.Tuple(ts)), (List.map (fun t -> (Id.genid x, t)) ts))::l
                             | _ -> l)
            []
            zts in
  let a = List.fold_left
            (fun zts (xt, yts) -> List.map (fun zt -> if zt = [xt] then yts else zt) zts)
            (List.map (fun x -> [x]) zts)
            r in
  (List.concat a, r, List.map (List.map snd) a)


(* gで使う補助関数 *)
let rec gsup = function
  | Type.Fun(ts, t) ->
      let (yts,_,_) = h_args (List.map (fun t -> (Id.genid "", t)) ts) in
      Type.Fun(List.map (fun x -> gsup (snd x)) yts, gsup t)
  | Type.Tuple(ts) -> Type.Tuple(List.map gsup ts)
  | Type.Array(t) -> Type.Array(gsup t)
  | t -> t
let gsup2 (a,r) (c,d) =
  match d with
  | [_] -> (a@[c], r)
  | l -> let xts = List.map (fun t -> (Id.genid c, gsup t)) l in
         (a@(List.map fst xts), (xts, c)::r)
(* プログラム本体の関数呼び出しの引数のタプルを展開する関数 *)
let rec g env = function
  | Let((x, Type.Fun(ts, t)), exp, e) ->
      let (yts, _, yenv) = h_args (List.map (fun t -> (Id.genid x, t)) ts) in
      let env' = M.add x (yenv, []) env in
      concat (g' env exp) (x, Type.Fun(List.map (fun x -> gsup (snd x)) yts, gsup t)) (g env' e)
  | Let((x,t), exp, e) -> concat (g' env exp) (x,gsup t) (g env e)
  | MakeCls((x, Type.Fun(_,t)), {entry = Id.L(l); actual_fv = fvs}, e) ->
      let (yenv, zenv) = M.find l env in
      let (fvs', r) = List.fold_left
	               gsup2
	               ([], [])
	               (List.combine fvs zenv) in
      List.fold_left
	(fun a (xts, c) -> LetTuple(xts, c, a))
        (MakeCls((x, Type.Fun(List.map gsup (List.concat yenv), gsup t)), {entry = Id.L(l); actual_fv = fvs'}, g (M.add x (yenv, []) env) e))
	r
  | MakeCls(_, _, _) -> assert false
  | LetTuple(xts, y, e) ->
      let a = List.fold_left
	        (fun a (x,t) -> match t with
		| Type.Fun(ts,_) ->
		    let (_, _, yenv) = h_args (List.map (fun t -> (Id.genid x, t)) ts) in
		    (x,(yenv,[]))::a
		| _ -> a)
	        []
	        xts in
      let xts' = List.map (fun (x,t) -> (x,gsup t)) xts in
      LetTuple(xts', y, g (M.add_list a env) e)
  | LetList((x, Type.Fun(ts, t)), y, e) ->
      let (yts, _, yenv) = h_args (List.map (fun t -> (Id.genid "fl", t)) ts) in
      let env' = M.add_list (List.map (fun p -> (p, (yenv, []))) (Syntax.matcher_variables x)) env in
      LetList((x, Type.Fun(List.map (fun x -> gsup (snd x)) yts, gsup t)), y, g env' e)
  | LetList((x,t), y, e) -> LetList((x,gsup t), y, g env e)
  | Ans(exp) -> g' env exp
and g' env = function
  | IfEq(x, y, e1, e2) -> Ans(IfEq(x, y, g env e1, g env e2))
  | IfLE(x, y, e1, e2) -> Ans(IfLE(x, y, g env e1, g env e2))
  | IfLT(x, y, e1, e2) -> Ans(IfLT(x, y, g env e1, g env e2))
  | IfNil(x, e1, e2) -> Ans(IfNil(x, g env e1, g env e2))
  | AppCls(x, args) ->
      let (yenv, _) = M.find x env in
      let (args', r) = List.fold_left
	                 gsup2
	                 ([], [])
	                 (List.combine args yenv) in
      List.fold_left
	(fun a (xts, c) -> LetTuple(xts, c, a))
        (Ans(AppCls(x, args')))
	r
  | AppDir(Id.L(x), args) when M.mem x env ->
      let (yenv, _) = M.find x env in
      let (args', r) = List.fold_left
	                 gsup2
	                 ([], [])
	                 (List.combine args yenv) in
      List.fold_left
	(fun a (xts, c) -> LetTuple(xts, c, a))
        (Ans(AppDir(Id.L(x), args')))
	r
  | exp -> Ans(exp)



(* 関数の引数や自由変数のタプルを展開する関数.引数の場合小さいもの優先 *)
let h env { name = (Id.L(x), t); args = yts; formal_fv = zts; body = e } =
  match t with
  | Type.Fun(_,b) ->
  let (yts', yunf, yenv) = h_args yts in
  let (zts', zunf, zenv) = h'' zts in
  let env' = M.add x (yenv,zenv) env in
  let a =
    List.fold_left
      (fun a (x,t) -> match t with
      | Type.Fun(ts,_) ->
	  let (_, _, yenv) = h_args (List.map (fun t -> (Id.genid x, t)) ts) in
	  (x,(yenv,[]))::a
      | _ -> a)
      []
      (yts'@zts') in
  let env'' = M.add_list a env' in
  let e' = List.fold_left
            (fun x (yt, zts) -> Let(yt, Tuple(List.map fst zts), x))
            e
            (yunf@zunf) in
  ({ name = (Id.L(x), Type.Fun(List.map (fun x -> gsup (snd x)) yts', gsup b)); args = List.map (fun (y,t) -> (y, gsup t)) yts'; formal_fv = List.map (fun (z,t) -> (z, gsup t)) zts'; body = g env'' e' }, env')
  | _ -> assert false


(* グローバル配列の型の引数タプルの展開と,グローバルクロージャの自由変数タプルの展開 *)
let i env { gname = (Id.L(x), t); length = l } =
  let l' =
    try
      List.length (List.concat (snd (M.find x env))) + 1
    with Not_found -> l in
  { gname = (Id.L(x), gsup t); length = l' }


(* 本体 *)
let f (Prog(globals, toplevel, e)) =
  Format.eprintf "unfolding tuples...@.";
  let (toplevel', env) =
    List.fold_left
      (fun (x, y) fu -> let (a,b) = h y fu in (x@[a], b))
      ([], M.empty)
      toplevel in
  Prog(List.map (i env) globals, toplevel', g env e)
