open Closure
(* ネストした組を平坦化するモジュール *)


(* 型を受け取り,その中のネストしたタプルを平坦化する関数 *)
let rec flat' = function
  | [] -> []
  | (Type.Tuple (ys,_))::xs -> flat' (ys@xs)
  | x::xs -> flat x::flat' xs
and flat = function
  | Type.Tuple(ts,b) -> Type.Tuple(flat' ts, b)
  | Type.Fun(xs, y,b) -> Type.Fun(List.map flat xs, flat y, b)
  | Type.Array(t,b) -> Type.Array(flat t, b)
  | t -> t


(* gで使う補助関数。変数と型の連想リストを受け取り,タプル型を展開
   返り値の第2引数は,展開された変数の集合 *)
let rec g' r tup = function
  | [] -> (List.rev r, tup)
  | (x, Type.Tuple(y,b))::xs ->
      let yts = List.map (fun t -> (Id.genid x, t)) y in
      g' r (((x, flat (Type.Tuple(y,b))), yts)::tup) (yts@xs)
  | (x, t)::xs -> g' ((x, flat t)::r) tup xs
(* 本体 *)    
let rec g env = function
  | IfEq(x, y, e1, e2) -> IfEq(x, y, g env e1, g env e2)
  | IfLE(x, y, e1, e2) -> IfLE(x, y, g env e1, g env e2)
  | IfLT(x, y, e1, e2) -> IfLT(x, y, g env e1, g env e2)
  | Let((x, t), e1, e2) -> Let((x, flat t), g env e1, g (M.add x t env) e2)
  | MakeCls((x, t), cl, e) -> MakeCls((x, flat t), cl, g (M.add x t env) e)
  | Tuple(xs) -> (* タプルの平坦化 *)
      let xts = List.map (fun x -> (x, M.find x env)) xs in
      let (xts', tup) = g' [] [] xts in
      List.fold_left
	(fun x (yt, zts) -> LetTuple(zts, fst yt, x))
        (Tuple(List.map fst xts'))
	tup
  | LetTuple(xts, y, e) ->
      let (xts', tup) = g' [] [] xts in
      let e' =
	List.fold_left
	  (fun x (yt, zts) -> Let(yt, Tuple(List.map fst zts), x))
	  (g (M.add_list xts env) e)
          (List.rev tup) in
      LetTuple(xts', y, e')
  | e -> e




(* 関数のネストした組の平坦化.引数と自由変数のネストを解除 *)
let h { name = (Id.L(x), t); args = yts; formal_fv = zts; body = e } =
  let yts' = List.map (fun (y, t) -> (y, flat t)) yts in
  let zts' = List.map (fun (z, t) -> (z, flat t)) zts in
  let e'   = g (M.add x t (M.add_list (yts@zts) M.empty)) e in
  { name = (Id.L(x), flat t); args = yts'; formal_fv = zts'; body = e' }



let f (Prog(toplevel, e)) =
  Format.eprintf "flattening nested tuples...@.";
  Prog(List.map h toplevel, g M.empty e)
