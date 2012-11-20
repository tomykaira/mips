open Closure
(* ネストした組を平坦化するモジュール *)


(* 型を受け取り,その中のネストしたタプルを平坦化する関数 *)
let rec flat' = function
  | [] -> []
  | (Type.Tuple (ys))::xs -> flat' (ys@xs)
  | x::xs -> flat x::flat' xs
and flat = function
  | Type.Tuple(ts) -> Type.Tuple(flat' ts)
  | Type.Fun(xs, y) -> Type.Fun(List.map flat xs, flat y)
  | Type.Array(t) -> Type.Array(flat t)
  | Type.List(t) as lt ->
      (match !t with
      | Some t' -> t := Some (flat t'); Type.List(t)
      | _ -> lt)
  | t -> t


(* gで使う補助関数。変数と型の連想リストを受け取り,タプル型を展開
   返り値の第2引数は,展開された変数の集合 *)
let rec gsup r tup = function
  | [] -> (List.rev r, tup)
  | (x, Type.Tuple(y))::xs ->
      let yts = List.map (fun t -> (Id.genid x, t)) y in
      gsup r (((x, flat (Type.Tuple(y))), yts)::tup) (yts@xs)
  | (x, t)::xs -> gsup ((x, flat t)::r) tup xs
(* 本体 *)    
let rec g env = function
  | Let((x, t), exp, e) -> concat (g' env exp) (x, flat t) (g (M.add x t env) e)
  | MakeCls((x, t), cl, e) -> MakeCls((x, flat t), cl, g (M.add x t env) e)
  | LetTuple(xts, y, e) ->
      let (xts', tup) = gsup [] [] xts in
      let e' =
	List.fold_left
	  (fun x (yt, zts) -> Let(yt, Tuple(List.map fst zts), x))
	  (g (M.add_list xts env) e)
          (List.rev tup) in
      LetTuple(xts', y, e')
  | LetList((x, t), y, e) -> LetList((x, flat t), y, g (M.add_list (List.map (fun p -> (p, t)) (Syntax.matcher_variables x)) env) e)
  | Ans(exp) -> g' env exp
and g' env = function 
  | IfEq(x, y, e1, e2) -> Ans(IfEq(x, y, g env e1, g env e2))
  | IfLE(x, y, e1, e2) -> Ans(IfLE(x, y, g env e1, g env e2))
  | IfLT(x, y, e1, e2) -> Ans(IfLT(x, y, g env e1, g env e2))
  | IfNil(x, e1, e2) -> Ans(IfNil(x, g env e1, g env e2))
  | Tuple(xs) -> (* タプルの平坦化 *)
      let xts = List.map (fun x -> (x, M.find x env)) xs in
      let (xts', tup) = gsup [] [] xts in
      List.fold_left
	(fun x (yt, zts) -> LetTuple(zts, fst yt, x))
        (Ans(Tuple(List.map fst xts')))
	tup
  | e -> Ans(e)




(* 関数のネストした組の平坦化.引数と自由変数のネストを解除 *)
let h { name = (Id.L(x), t); args = yts; formal_fv = zts; body = e } =
  let yts' = List.map (fun (y, t) -> (y, flat t)) yts in
  let zts' = List.map (fun (z, t) -> (z, flat t)) zts in
  let e'   = g (M.add x t (M.add_list (yts@zts) M.empty)) e in
  { name = (Id.L(x), flat t); args = yts'; formal_fv = zts'; body = e' }



let f (Prog(toplevel, e)) =
  Format.eprintf "flattening nested tuples...@.";
  Prog(List.map h toplevel, g M.empty e)
