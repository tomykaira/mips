open ANormal

(* エイリアス解析の結果を用いて無駄なロード・ストアを削除するモジュール *)


(* インデックスとそこに入ってる変数の連想集合 *)
module M' =
  Map.Make
    (struct
      type t = int
      let compare = compare
     end)


(* 配列の中の状態を表す型 *)
type state =
  | A of Id.t M'.t * Id.t      (* 全ての状態が分かっている *)
  | B of Id.t M.t * Id.t M'.t  (* 一部の状態が分かっている *)
let show = function
  | A (a,b) -> "A " ^ M'.fold (fun x y st-> st ^ Printf.sprintf "(%d %s)" x y) a "" ^ b
  | B (a,b) -> "B " ^ M.fold (fun x y st -> st ^ Printf.sprintf "(%s %s)" x y) a "" ^ M'.fold (fun x y st -> st ^ Printf.sprintf "(%d %s)" x y) b "" 

let unknown = B (M.empty, M'.empty)

let munion m n =
  let inter x y = match x, y with
  | A (a,b) ,A(c,d) ->
      let g _ p q = match p, q with
      | Some x, Some y when x = y -> Some(x)
      | Some x, _ when x = d -> Some(x)
      | _ , Some x when b = x -> Some(x)
      | _ -> None in
      if b = d then A(M'.merge g a c, d) else B(M.empty, M'.merge g a c)
  | A (a,b), B (_,d) | B (_,d), A (a,b) ->
      let g _ p q = match p, q with
      | Some x, Some y when x = y -> Some(x)
      | _ , Some x when b = x -> Some(x)
      | _ -> None in
      let q = M'.merge g a d in
      B (M.empty,q)
  | B (a,b), B (c,d) ->
      let g _ p q = match p, q with
      | Some x, Some y when x = y -> Some(x)
      | _ -> None in
      let p = M.merge g a c in
      let q = M'.merge g b d in
      B (p,q)
  | _ -> unknown in
  let f _ a b = match a, b with
  | Some (a,x), Some (_,y) -> Some(a,inter x y)
  | Some (a,_), _ | _, Some(a,_) -> Some(a,unknown)
  | _ , _-> None in
  M.merge f m n


(*  メモリを操作する関数群 *)
let findt x mem = let (t,_,_) = fst (M.find x mem) in t
let finda x mem = let (_,y,_) = fst (M.find x mem) in y
let findc x mem = let (_,_,z) = fst (M.find x mem) in z
let finds x mem = snd (M.find x mem)
let ovwr x y mem = let (p,_) = M.find x mem in M.add x (p,y) mem
let addmemi x i y mem =
  match M.find x mem with
  | p, A (a,b) -> M.add x (p, A(M'.add i y a,b)) mem
  | p, B (_,b) -> M.add x (p, B(M.empty,M'.add i y b)) mem
let addmemv x y z mem =
  match M.find x mem with
  | p, B (a,b) when M.mem y a -> M.add x (p, B(M.add y z a,b)) mem
  | p, _ -> M.add x (p, B(M.singleton y z,M'.empty)) mem
    
let filter env mem =
  let f = function
    | p, A(a,b) when S.mem b env -> (p, A(M'.filter (fun _ x -> S.mem x env) a,b))
    | p, A(a,_) -> (p, B(M.empty, M'.filter (fun _ x -> S.mem x env) a))
    | p, B(a,b) -> (p, B(M.filter (fun x y -> S.mem x env && S.mem y env) a,
		  M'.filter (fun _ x -> S.mem x env) b)) in
  M.map f mem

(* 命令列を順方向に走査して、put -> get 及び 同じものの put を削除する関数
   返り値の2番目は必ず決まった位置にある変数の集合 *)
let rec g dest env mem const funs = function
  | Let((x,t), exp, e) ->
      let env' = S.add x env in
      let (exp', mem') = g' x env' mem const funs exp in
      let const' = match exp with
      | Int(i) -> M.add x i const
      | _ -> const in
      let (e', mem'') = g dest env' (filter env' mem') const' funs e in
      (Let((x,t), exp', e'), mem'')
  | LetRec({ name = (x,t); args = yts; body = e1 }, e2) ->
      let ret = Id.genid "dest" in
      let env' = S.add x env in
      let ys = List.map fst yts in
      let zs = S.elements (S.filter (fun x -> M.mem x mem) (fv e1)) in
      let (retarray, t2) =
	match t with Type.Fun(_,(Type.Array _ as t)) -> (true,t) | _ -> (false,Type.Unit) in
      let data = Id.genid "data" in
      let new_node = ((t2,S.empty,S.empty),unknown) in
      let allknown_mem =
	(fun a -> if retarray then M.add ret new_node a else a)
	  (M.map (fun (x,_) -> (x,A(M'.empty,data))) mem) in
      let env'' = S.add_list ys (S.add data env') in
      let rec f af =
        let (_, af') = g ret env'' allknown_mem const (M.add x (data,ret,ys,zs,af) funs) e1 in
	if af = af' then af else f af' in
      let after = f allknown_mem in
      let funs' = M.add x (data,ret,ys,zs,after) funs in
      let empty_mem =
	M.map (fun (x,_) -> (x,unknown))
	  (if retarray then M.add ret new_node mem else mem) in
      let (e1', m) =  g ret (S.add_list ys env') empty_mem const funs' e1 in
      let (e2', mem') =  g dest env' mem const funs' e2 in
      (LetRec({ name = (x,t); args = yts; body = e1' }, e2'), mem')
  | LetTuple(xts,y,e) ->
      let (e', mem') = g dest (S.add_list (List.map fst xts) env) mem const funs e in
      (LetTuple(xts,y,e'), mem')
  | LetList((m,t),y,e) ->
      let (e', mem') = g dest (S.add_list_matcher m env) mem const funs e in
      (LetList((m,t),y,e'), mem')
  | Ans(exp) ->
      let (exp', mem) = g' dest env mem const funs exp in
      (Ans(exp'), mem)
and g' dest env mem const funs = function
  | IfEq(x,y,e1,e2) ->
      let (e1', mem1) = g dest env mem const funs e1 in
      let (e2', mem2) = g dest env mem const funs e2 in
      (IfEq(x,y,e1',e2'), munion mem1 mem2)
  | IfLE(x,y,e1,e2) ->
      let (e1', mem1) = g dest env mem const funs e1 in
      let (e2', mem2) = g dest env mem const funs e2 in
      (IfLE(x,y,e1',e2'), munion mem1 mem2)
  | IfLT(x,y,e1,e2) ->
      let (e1', mem1) = g dest env mem const funs e1 in
      let (e2', mem2) = g dest env mem const funs e2 in
      (IfLT(x,y,e1',e2'), munion mem1 mem2)
  | IfNil(x,e1,e2) ->
      let (e1', mem1) = g dest env mem const funs e1 in
      let (e2', mem2) = g dest env mem const funs e2 in
      (IfNil(x,e1',e2'), munion mem1 mem2)
  | App(x,ys) as exp when M.mem x funs ->
      let (data,ret,zs,ws,after) = M.find x funs in
      let (ys', zs') =
	if M.mem dest mem && M.mem ret after then (dest::ys,ret::zs)
	else (ys,zs) in
      let en1 = List.fold_left2 (fun en x y -> M.add x y en) (M.empty) zs' ys' in
      let en2 = List.fold_left2 (fun en x y -> M.add x y en) (M.empty) ys' zs' in
      let ren en x = try M.find x en with Not_found -> x in
      (* この関数適用の後にメモリの状態がどう変わるか *)
      let memchange =
	M.mapi
	  (fun x a -> try 
	    let (y,z) = M.find (ren en2 x) after in
	    match z with
	    | A(a,b) -> (y, A(M'.map (ren en1) a, ren en1 b))
	    | B(a,b) -> (y, B(M.map (ren en1) a, M'.map (ren en1) b))
	  with Not_found -> a)
	  mem in
      (* メモリの状態 *)
      let mem' =
	M.mapi
	  (fun x (y,z) ->
	    let z' =
	      match z, finds x memchange with
	      | A(a,b), A(c,d) when d = data ->
		  A(M'.fold (fun p q a -> M'.add p q a) c a, b)
	      | _, A(c,d) -> A(c, d)
	      | _, B(c,d) -> B(c, d)
	      | B(a,b), B(c,d) ->
		  let g _ p q = match p, q with
	          | Some x, Some y when x = y -> Some(x)
	          | _ -> None in
	          let p = M.merge g a c in
	          let q = M'.merge g b d in
	          B (p,q) in
	    (y,z'))
	  mem in
      (* 状態の変わったメモリとエイリアス関係にあるメモリの状態はunknown *)
      let un =
	M.fold
	  (fun x ((_,y,_),_) un ->
	    match finds x memchange with
	    | A(a,b) when M'.is_empty a && b = data -> un
	    | _ ->
		S.fold
		  (fun p un -> S.add p un)
	          y
	          un)
	  memchange
	  S.empty in
      (* 引数,自由変数となったメモリの子孫の状態はunknownにしてしまう *)
      let un' =
	(* unknownにする配列を集める *)
	let rec f x ret =
	  if not (M.mem x mem') || S.mem x ret then ret else
	  let ret = S.add x ret in
	  let z = findc x mem in
	  S.fold (fun y ret -> f y ret) z ret in
        List.fold_left
	  (fun c y ->
	    try S.fold (fun y c -> f y c) (findc y mem') c with Not_found -> c)
	    un
	    (ys@ws) in
      let mem'' =
	S.fold (fun x mem -> ovwr x unknown mem) un' mem' in
      (exp, (*mem''*) M.map (fun (y,_) -> (y,unknown)) mem)
  | Get(x,y) as exp->
      let st = finds x mem in
      (match st with
      | A (a, b) ->
	  if M'.is_empty a then (Var(b), mem)
	  else if M.mem y const then
	    if M'.mem (M.find y const) a then (Var(M'.find (M.find y const) a), mem)
	    else (Var(b), mem)
	  else (exp, ovwr x (B(M.singleton y dest, M'.empty)) mem)
      | B (a, b) ->
	  if M.mem y const && M'.mem (M.find y const) b then
	    (Var(M'.find (M.find y const) b), mem)
	  else if M.mem y a then (Var(M.find y a), mem)
	  else
	    let mem' =
	      if M.mem y const then
		ovwr x (B(a,M'.add (M.find y const) dest b)) mem
 	      else ovwr x (B(M.add y dest a,b)) mem in
	    (exp, mem'))
  | Put(x,y,z) as exp ->
      let st = finds x mem in
      (match st with
      | A (a,b) ->
	  if M.mem y const then
	    let yi = M.find y const in
	    let z' = if M'.mem yi a then M'.find yi a else b in
	    if z = z' then (Unit, mem)
	    else
	      let mem' = S.fold (fun t mem -> ovwr t unknown mem) (finda x mem) (ovwr x (A(M'.add yi z a,b)) mem) in
	      (exp, mem')
	  else
	    if M'.is_empty a && b = z then (Unit, mem)
	    else
	      let mem' = S.fold (fun t mem -> ovwr t unknown mem) (finda x mem) (ovwr x (B(M.singleton y z, M'.empty)) mem) in
	      (exp, mem')
      | B (a,b) ->
	  if M.mem y const then
	    let yi = M.find y const in
	    if M'.mem yi b && M'.find yi b = z then (Unit, mem)
	    else
	      let mem' = S.fold (fun t mem -> ovwr t unknown mem) (finda x mem) (ovwr x (B(a,M'.add yi z b)) mem) in
	      (exp, mem')
	  else if M.mem y a && M.find y a = z then (Unit, mem)
	  else
	    let mem' = S.fold (fun t mem -> ovwr t unknown mem) (finda x mem) (ovwr x (B(M.singleton y z, M'.empty)) mem) in
	    (exp,mem'))
  | ExtFunApp(("create_array"|"create_float_array"|"create_tuple_array"),_::x::_) as exp ->
      (exp, ovwr dest (A(M'.empty, x)) mem)
  | ExtFunApp(_,ys) | App(_,ys) as exp ->
      let mem' =
	List.fold_left
	  (fun mem y ->
	    if M.mem y mem then
	      S.fold
		(fun t mem -> ovwr t unknown mem)
		(finda y mem)
		(ovwr y unknown mem)
	    else mem)
	  mem
	  ys in
      (exp, mem')
  | exp -> (exp, mem)



let f gr e =
(*   M.iter (fun x (t,k,l) -> Format.eprintf "%s : " x; Format.eprintf "%s/" (Type.show t);S.iter (Format.eprintf "%s ") k; Format.eprintf "/"; S.iter (Format.eprintf "%s ") l; Format.eprintf "@.") gr; *)
  let mem = M.map (fun x -> (x, unknown)) gr in
  Format.eprintf "eliminating loads...@.";
  fst (g (Id.genid "dest") S.empty mem M.empty M.empty e)
