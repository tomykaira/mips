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

let er find x env = try find x env with Not_found -> Format.eprintf "%s : Not_found@." x; failwith "Not_found"
(*  メモリを操作する関数群 *)
let findg x mem = fst (M.find x mem)
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
      let (exp', mem') = g' x env mem const funs exp in
      let const' = match exp with
      | Int(i) -> M.add x i const
      | _ -> const in
      let (e', mem'') = g dest (S.add x env) (filter env mem') const' funs e in
      (Let((x,t), exp', e'), mem'')
  | LetRec({ name = (x,t); args = yts; body = e1 }, e2) ->
      let ret = Id.genid "dest" in
      let env' = S.add x env in
      let ys = List.map fst yts in
      let retarray =
	match t with Type.Fun(_,Type.Array _) -> true | _ -> false in
      let data = Id.genid "data" in
      let allknown_mem =
	(fun a -> if retarray then M.add ret (S.empty,unknown) a else a)
	  (M.map (fun (x,_) -> (x,A(M'.empty,data))) mem) in
      let rec f af =
        let (e1', af') = g ret (S.add_list ys env') allknown_mem const (M.add x (data,ret,ys,af) funs) e1 in
	if af = af' then af else f af' in
      let after = f allknown_mem in
      let funs' = M.add x (data,ret,ys,after) funs in
      let empty_mem =
	M.map (fun (x,_) -> (x,unknown))
	  (if retarray then M.add ret (S.empty,unknown) mem else mem) in
      let (e1', _) =  g ret (S.add_list ys env') empty_mem const funs' e1 in
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
  | App(x,ys) as exp ->
      let (data,ret,zs,after) = M.find x funs in
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
      (* 状態の変わったメモリのエイリアス仲間の状態はunknown *)
      let mem'' =
	M.fold
	  (fun x (y,_) mem ->
	    let f w =
	      match finds x memchange with
	      | A(a,b) when M'.is_empty a && b = data -> w
	      | _ -> unknown in
	    S.fold
	      (fun p mem -> ovwr p (f (finds p memchange)) mem)
	      y
	      mem)
	  memchange
	  mem' in
      (exp, mem'')
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
	      let mem' = S.fold (fun t mem -> addmemi t yi z mem) (findg x mem) (ovwr x (A(M'.add yi z a,b)) mem) in
	      (exp, mem')
	  else
	    if M'.is_empty a && b = z then (Unit, mem)
	    else
	      let mem' = S.fold (fun t mem -> addmemv t y z mem) (findg x mem) (ovwr x (B(M.singleton y z, M'.empty)) mem) in
	      (exp, mem')
      | B (a,b) ->
	  if M.mem y const then
	    let yi = M.find y const in
	    if M'.mem yi b && M'.find yi b = z then (Unit, mem)
	    else
	      let mem' = S.fold (fun t mem -> addmemi t yi z mem) (findg x mem) (ovwr x (B(a,M'.add yi z b)) mem) in
	      (exp, mem')
	  else if M.mem y a && M.find y a = z then (Unit, mem)
	  else
	    let mem' = S.fold (fun t mem -> addmemv t y z mem) (findg x mem) (ovwr x (B(M.singleton y z, M'.empty)) mem) in
	    (exp,mem'))
  | ExtFunApp(("create_array"|"create_float_array"|"create_tuple_array"),_::x::_) as exp ->
      (exp, ovwr dest (A(M'.empty, x)) mem)
  | ExtFunApp(_,ys) as exp ->
      let mem' =
	List.fold_left
	  (fun mem y ->
	    if M.mem y mem then
	      S.fold
		(fun t mem -> ovwr t unknown mem)
		(findg y mem)
		(ovwr y unknown mem)
	    else mem)
	  mem
	  ys in
      (exp, mem')
  | exp -> (exp, mem)



let f gr e =
(*  M.iter (fun x l -> Format.eprintf "%s : " x; S.iter (Format.eprintf "%s ") l; Format.eprintf "@.") gr; *)
  let dest = Id.genid "dest" in
  let gr = M.add dest S.empty gr in
  let mem = M.map (fun x -> (x, unknown)) gr in
  Format.eprintf "eliminating loads...@.";
  fst (g (Id.genid "dest") S.empty mem M.empty M.empty e)
