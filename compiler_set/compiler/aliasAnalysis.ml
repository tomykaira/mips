open ANormal

(* エイリアス解析と、それを用いた最適化を行うモジュール *)

(* 解析結果のグラフ *)
let graph : (Type.t * S.t * int list option M.t) M.t ref = ref M.empty


let nub a =
  let rec f = function
    | x::y::xs when x = y -> f (y::xs)
    | x::xs -> x::f xs
    | xs -> xs in
  f (List.sort compare a)



(* 新しいノードの追加 *)
let add_node x t = graph := M.add x (t,S.empty,M.empty) !graph


(* 有向辺の追加 *)
let add_arrow'' y i q =
  let r =
    if not (M.mem y q) then None else
    match M.find y q, i with
    | Some(a), Some(b) -> Some(nub (a@b))
    | _, _ -> None in
  M.add y r q
let add_arrow' x (t,p,q) y i =
    graph := M.add x (t,p,add_arrow'' y i q) !graph
let add_arrow x y i =
  if not (M.mem x !graph) then () else
  add_arrow' x (M.find x !graph) y i 



(* エイリアス関係の追加 *)
let add_alias' x (t1,p1,q1) y (t2,p2,q2) =
  (* xとyの間に無向辺をひく *)
  graph := M.add y (t2, S.add x p2, q2) (M.add x (t1, S.add y p1, q1) !graph);
  (* xの親にyを子として追加.逆も *)
  graph :=
    M.map
      (fun (t,p,q) ->
	if not (M.mem x q || M.mem y q) then (t,p,q) else
	let i = if not (M.mem x q && M.mem y q) then None else
	match M.find x q, M.find x q with
	| Some(a), Some(b) -> Some(nub (a@b))
	| _, _ -> None in
	(t,p,M.add y i (M.add x i q)))
      !graph;
  (* xの子はyの子でもある。逆も. *)
  let q = M.fold (fun y i q -> add_arrow'' y i q) q1 q2 in
  graph := M.add y (t2, p2, q) (M.add x (t1, p1, q) !graph)
let add_alias x y =
  if not (M.mem x !graph && M.mem y !graph) then () else
  add_alias' x (M.find x !graph) y (M.find y !graph) 
  


(* 2つのインデックスに共通部分があるか調べる *)
let iinter x y =
  match x, y with
  | Some(i), Some(j) when not (List.exists (fun x -> List.mem x j) i) -> false
  | _ -> true

(* 親子関係(親の中に子が入る可能性がある)の追加 *)
let add_child x i y =
  if not (M.mem x !graph && M.mem y !graph) then () else
  let (t1, p1, q1) = M.find x !graph in
  let (t2, p2, q2) = M.find y !graph in
  let l = (x,(t1,p1,q1))::S.fold (fun x l -> (x, M.find x !graph)::l) p1 [] in
  (* 親xとそのエイリアス仲間にyを子として追加 *)
  List.iter (fun (z,w) -> add_arrow' z w y i) l;
  (* 親x及びそのエイリアス仲間の,インデックスの一致する子はyとエイリアス関係にある *)
  List.iter
    (fun (_,(_,_,q)) ->
      M.iter (fun w j -> if iinter i j then add_alias' w (M.find w !graph) y (t2,p2,q2)) q)
    l


(* 型の合うものの間すべてにエイリアス関係及び親子関係を構築 *)
let add_all l =
  let l' = List.map (fun x -> (x, M.find x !graph)) (List.filter (fun x -> M.mem x !graph) l) in
  List.iter
    (fun (x1,(t1,p1,q1)) ->
      List.iter (fun (x2,(t2,_,_)) ->
	(if x1 = x2 then () else
	if t1 = t2 then graph := M.add x1 (t1,S.add x2 p1,q1) !graph);
	match t1, t2 with
	| Type.Tuple(ts), _ when List.mem t2 ts ->
	    let (i,_) = List.fold_left (fun (i,n) t -> ((if t = t2 then n::i else i), n+1)) ([],0) ts in
	    add_child x1 (Some i) x2
	| Type.Array(t), _ when t = t2 -> add_child x1 None x2
	| Type.List(t), Type.List(t') when !t = !t' -> add_child x1 (Some [1]) x2
	| Type.List(t), _ when !t = Some(t2) -> add_child x1 (Some [0]) x2
	| _ -> ()) l')
    l'


(* 部分グラフを作る *)
let part l gr =
  M.map
    (fun (t,p,q) -> (t, S.inter l p, M.filter (fun x _ -> S.mem x l) q))
    (M.filter (fun x _ -> S.mem x l) gr)

(* 型の中にArrayが現れるか判定(関数除く) *)
let rec earray = function
  | Type.Array _ -> true
  | Type.Tuple(ts) -> List.exists earray ts
  | Type.Var(t) | Type.List(t) ->
      (match !t with
      | Some(t') -> earray t'
      | None -> false)
  | _ -> false

(* エイリアス解析を行う関数.
   解析結果はノード(配列かそれをもつ変数)と,型とそこから出る2種類のエッジ(メモリ上の
   同じところ(完全に同じか重なりもしないかどっちか)をさしうる関係(エイリアス関係)を
   表す無向辺と、a[i]にbが入りうる関係(親子関係)を表す有向辺(a->b,i))の集合の組の
   連想集合である. *)
let rec analyse dest funs const = function
  | Let ((x,t), exp, e) ->
      if earray t then add_node x t;
      analyse' x funs const exp;
      let const' = match exp with
      | Int(i) -> M.add x i const
      | _ -> const in
      analyse dest funs const' e
  | LetRec({ name = (x,t); args = yts; body = e1 }, e2) ->
      let (t1,t2) = match t with
      | Type.Fun(a,b) -> (a,b)
      | _ -> assert false in
      if not (List.exists earray t1 || earray t2) then
	(analyse dest funs const e1; analyse dest funs const e2) else
      let graph_backup = !graph in
      let ret = Id.genid "ret" in
      let ys = List.map fst yts in
      let add_nodes l gr =
	List.fold_left
	  (fun gr (x,t) -> if earray t then M.add x (t,S.empty,M.empty) gr else gr)
	  gr
	  l in
      let graph' = add_nodes ((ret,t2)::yts) !graph in
      let vars = S.add ret (S.union (S.of_list (List.map fst (List.filter (fun (_,t) -> earray t) yts))) (S.filter (fun x -> M.mem x !graph) (fv e1))) in
      let rec f gr =
        graph := graph';
	analyse ret (M.add x (ret,ys,gr) funs) const e1;
	let gr' = part vars !graph in
	if gr = gr' then gr else
	f gr' in
      let gr = f (add_nodes ((ret,t2)::yts) M.empty) in
      graph := graph_backup;
      let funs' = M.add x (ret,ys,gr) funs in
      analyse dest funs' const e1;
      analyse dest funs' const e2
  | LetTuple(xts, y, e) ->
      let fvs = fv e in
      ignore (List.fold_left
		(fun n (x,t) -> if earray t && S.mem x fvs then (add_node x t; add_child y (Some [n]) x); n+1)
		0
		xts);
      analyse dest funs const e
  | LetList((xs,t), y, e) ->
      (match xs with
      | Syntax.ListWithNil [x] -> if earray t then (add_node x t; add_child y (Some [0]) x)
      | Syntax.ListWithoutNil [x] -> if earray t then (add_node x (Type.List(ref (Some(t)))); add_alias x y)
      | Syntax.ListWithNil (x::s) ->
	  let a = Id.genid y in
	  analyse dest funs const (LetTuple([(x,t);(a,Type.List(ref (Some(t))))],y,LetList((Syntax.ListWithNil s,t),a, e)))
      | Syntax.ListWithoutNil (x::s) ->
	  let a = Id.genid y in
	  analyse dest funs const (LetTuple([(x,t);(a,Type.List(ref (Some(t))))],y,LetList((Syntax.ListWithoutNil s,t),a, e)))
      | _ -> assert false)
  | Ans(exp) -> analyse' dest funs const exp
and analyse' dest funs const = function
  | IfEq(_,_,e1,e2) | IfLE(_,_,e1,e2) | IfLT(_,_,e1,e2) | IfNil(_,e1,e2) ->
      analyse dest funs const e1; analyse dest funs const e2
  | Var(x) -> add_alias dest x
  | App(x, ys) ->
      if not (M.mem x funs) then () else
      let (ret, zs, gr) = M.find x funs in
      let env = List.fold_left2 (fun env x y -> M.add x y env) (M.singleton ret dest) zs ys in
      M.iter
	(fun x (_,p,q) ->
	  let y = try M.find x env with Not_found -> x in
	  S.iter (fun z -> add_alias y (try M.find z env with Not_found -> z)) p;
	  M.iter (fun z i -> add_child y i (try M.find z env with Not_found -> z)) q)
	gr
  | Tuple(xs) ->
      ignore(List.fold_left
	       (fun n x -> add_child dest (Some [n]) x; n+1)
	       0
	       xs)
  | Get(x,y) ->
      (try add_child x (Some [M.find y const]) dest
      with Not_found -> add_child x None dest)
  | Put(x,y,z) ->
      (try add_child x (Some [M.find y const]) z
      with Not_found -> add_child x None z)
  | ExtFunApp(_,ys) -> add_all (dest::ys)
  | Cons(x,y) -> add_child dest (Some [0]) x; add_child dest (Some [1]) y
  | _ -> ()


let f e =
  Format.eprintf "Alias Analysis...@.";
  let x = Id.genid "ans" in
  analyse x M.empty M.empty e;
  e
