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
let rec g dest env mem const = function
  | Let((x,t), exp, e) ->
      let env' = S.add x env in
      let (exp', mem') = g' x env' mem const exp in
      let const' = match exp with
      | Int(i) -> M.add x i const
      | _ -> const in
      let (e', mem'') = g dest env' (filter env' mem') const' e in
      (Let((x,t), exp', e'), mem'')
  | LetRec({ name = (x,t); args = yts; body = e1 }, e2) ->
      let ret = Id.genid "dest" in
      let env' = S.add x env in
      let empty_mem =
	let mem' = M.map (fun (x,_) -> (x,unknown)) mem in
	match t with
	| Type.Fun(_,(Type.Array _ as t)) ->
	    M.add ret ((t,S.empty,S.empty),unknown) mem'
	| _ -> mem' in
      let (e1', _) = g ret (S.add_list (List.map fst yts) env') empty_mem const e1 in
      let (e2', mem') =  g dest env' mem const e2 in
      (LetRec({ name = (x,t); args = yts; body = e1' }, e2'), mem')
  | LetTuple(xts,y,e) ->
      let (e', mem') = g dest (S.add_list (List.map fst xts) env) mem const e in
      (LetTuple(xts,y,e'), mem')
  | LetList((m,t),y,e) ->
      let (e', mem') = g dest (S.add_list_matcher m env) mem const e in
      (LetList((m,t),y,e'), mem')
  | Ans(exp) ->
      let (exp', mem) = g' dest env mem const exp in
      (Ans(exp'), mem)
and g' dest env mem const = function
  | IfEq(x,y,e1,e2) ->
      let (e1', mem1) = g dest env mem const e1 in
      let (e2', mem2) = g dest env mem const e2 in
      (IfEq(x,y,e1',e2'), munion mem1 mem2)
  | IfLE(x,y,e1,e2) ->
      let (e1', mem1) = g dest env mem const e1 in
      let (e2', mem2) = g dest env mem const e2 in
      (IfLE(x,y,e1',e2'), munion mem1 mem2)
  | IfLT(x,y,e1,e2) ->
      let (e1', mem1) = g dest env mem const e1 in
      let (e2', mem2) = g dest env mem const e2 in
      (IfLT(x,y,e1',e2'), munion mem1 mem2)
  | IfNil(x,e1,e2) ->
      let (e1', mem1) = g dest env mem const e1 in
      let (e2', mem2) = g dest env mem const e2 in
      (IfNil(x,e1',e2'), munion mem1 mem2)
  | Get(x,y) as exp ->
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
  | ExtFunApp(("xor"|"sqrt"|"not"|"print_char"|"input_char"|"read_char"),_) as exp ->      
      (* 後でコンパイラで展開される外部関数。
	 退避不要なのでここをまたぐ変数はスタックに置かれない *)
      (exp, mem)
  | App _ | ExtFunApp _ as exp ->
      (* メモリの状態を空っぽに *)
      (exp, M.map (fun (y,_) -> (y,unknown)) mem)
  | exp -> (exp, mem)



let f gr e =
(*   M.iter (fun x (t,k,l) -> Format.eprintf "%s : " x; Format.eprintf "%s/" (Type.show t);S.iter (Format.eprintf "%s ") k; Format.eprintf "/"; S.iter (Format.eprintf "%s ") l; Format.eprintf "@.") gr; *)
  let mem = M.map (fun x -> (x, unknown)) gr in
  Format.eprintf "eliminating loads...@.";
  fst (g (Id.genid "dest") S.empty mem  M.empty e)
