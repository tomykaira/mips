open Asm

(* グラフ彩色によるレジスタ割り当て *)

(* ノードの状態を示すデータ型 *)
type info =
  | Colored of Id.t
  | Prefer of Id.t list * Id.t list

(* グラフにノードを追加する関数 *)
let addn' x (l, s1, re1) m =
  try
    let (k, s2, re2) = M.find x m in
    M.add x ((remove_and_uniq S.empty (l@k)), S.union s1 s2, re1 + re2) m
  with Not_found -> M.add x (l, s1, re1) m
let addn x l g =
  List.fold_left
    (fun r y -> addn' y ([x], S.empty, 0) r)
    (addn' x (l, S.empty, 0) g)
    l 
(* 変数のリストから完全グラフを作る関数 *)
let perf l =
  List.fold_left
    (fun r y -> addn y (remove_and_uniq (S.singleton y) l) r)
    M.empty
    l
(* 同じ命令で使われる変数を追加する関数 *)
let addvar' x l m =
  try
    let (k, s, r) = M.find x m in
    M.add x (k, S.union s l, r) m
  with Not_found -> M.add x ([], l, 0) m
let addvar l m =
  List.fold_left
    (fun m x -> addvar' x (S.remove x (S.of_list l)) m)
    m
    l
(* 変数が参照される回数に+1する関数 *)
let addone' x m =
  try
    let (k, s, r) = M.find x m in
    M.add x (k, s, r+1) m
  with Not_found -> M.add x ([], S.empty, 1) m
let addone l m =
  List.fold_left (fun x y -> addone' y x) m l
(* 2つのグラフを合わせる関数 *)
let union g1 g2 = M.fold addn' g1 g2 
(* preferのグラフをいじる関数 *)
let addp x y p =
  if is_reg x || x.[0] = '%' || y = [] || (List.hd y).[0] = '%' then p else
  try let (a,b) = M.find x p in M.add x (y@a,b) p
  with Not_found -> M.add x (y,[]) p
let addh x y p =
  if is_reg x || x.[0] = '%' || y = [] || (List.hd y).[0] = '%' then p else
  try let (a,b) = M.find x p in M.add x (a,y@b) p
  with Not_found -> M.add x ([],y) p
let punion p1 p2 =
  M.fold
    (fun x (p,q) p2 ->
      try let (p',q') = M.find x p2 in M.add x (p@p',q@q') p2
      with Not_found -> M.add x (p,q) p2)
    p1
    p2
(* 式が分岐か判定 *)
let is_br = function
  | IfEq _ | IfLE _ | IfLT _ | IfFEq _ | IfFLE _ | IfFLT _ -> true
  | _ -> false 
(* 命令列(関数呼び出しなし)に対し整数,浮動小数のグラフを構成.グラフは,
   変数名と(隣接頂点,同じ命令で使われる変数の集合,参照される回数)の連想集合
   ついでにpreferの情報も集める *)
let rec make' vars rr rf dest cont = function
  | Ans(exp) -> make'' rr rf dest cont exp
  | Let((x,t) as xt, exp, e) ->
      let cont' = concat e dest cont in
      let rr' =
	match t with
	| Type.Float | Type.Unit -> rr
	| _ ->
	    let fvs = remove_and_uniq (S.singleton x) (fv_int cont') in
	    addn x fvs rr in
      let rf' =	
	match t with
	| Type.Float ->
	    let fvs = remove_and_uniq (S.singleton x) (fv_float cont') in
	    addn x fvs rf 
	| _ -> rf in
      let (rr'', rf'', p1) = make'' rr' rf' xt cont' exp in
      let vars' =
	if is_br exp then S.empty else
	match snd xt with
	| Type.Unit -> vars
	| Type.Float -> List.fold_left (fun vars x -> S.add x vars) vars (fv_float_exp exp)
	| _ -> List.fold_left (fun vars x -> S.add x vars) vars (fv_int_exp exp) in
      let (rr3, rf3, p2) = make' vars' rr'' rf'' dest cont e in
      (rr3, rf3, S.fold (fun v p -> addh v [x] p) vars' (addh x (S.elements vars') (punion p2 p1)))
and make'' rr rf dest cont = function
  | IfEq(x,y,e1,e2) | IfLE(x,y,e1,e2) | IfLT(x,y,e1,e2) ->
      let (rr1, rf1, p1) = make' S.empty rr rf dest cont e1 in
      let (rr2, rf2, p2) = make' S.empty rr rf dest cont e2 in
      let f t = addvar [x;y] (addone [x;y] t) in
      (f (union rr1 rr2), union rf1 rf2, punion p1 p2)
  | IfFEq(x,y,e1,e2) | IfFLE(x,y,e1,e2) | IfFLT(x,y,e1,e2) -> 
      let (rr1, rf1, p1) = make' S.empty rr rf dest cont e1 in
      let (rr2, rf2, p2) = make' S.empty rr rf dest cont e2 in
      let f t = addvar [x;y] (addone [x;y] t) in
      (union rr1 rr2, f (union rf1 rf2), punion p1 p2)
  | exp ->
      let fvi = fv_int_exp exp in
      let fvf = fv_float_exp exp in
      let fi t = addvar fvi (addone fvi t) in
      let ff t = addvar fvf (addone fvf t) in
      let prefer = match exp with
      | CallCls(_,x,ys,zs) ->
	  let rec f x y = match (x,y) with
	  | ([],_) | (_,[]) -> []
	  | (z::xs,w::ys) -> (z,[w])::f xs ys in
      addp x [reg_cl] (List.fold_left (fun x (y, z) -> addp y z x) (List.fold_left (fun x (y, z) -> addp y z x) M.empty (f ys allregs)) (f zs allfregs))
      | CallDir(_,ys,zs) ->
	  let rec f x y = match (x,y) with
	  | ([],_) | (_,[]) -> []
	  | (z::xs,w::ys) -> (z,[w])::f xs ys in
          List.fold_left (fun x (y, z) -> addp y z x) (List.fold_left (fun x (y, z) -> addp y z x) M.empty (f ys allregs)) (f zs allfregs)
      | AddI(y,0) | FMov(y) -> addp (fst dest) [y] (addp y [fst dest] M.empty)
      | _ -> M.empty in
      (fi rr, ff rf, prefer)
let make e =
  (* まず,一番最初に生きている変数で完全グラフを作る *)
  let rr = perf (fv_int e) in
  let rf = perf (fv_float e) in
  make' S.empty rr rf ("%g0", Type.Unit) (Ans(Nop)) e
	

(* グラフからコスト最小のものを選び出す関数 *)
let take_min g =
  M.fold
    (fun x (_,sc,_) (y,sc') ->
      if sc < sc' then (x,sc) else (y,sc'))
    g
    (let (x, (_,sc,_)) = M.choose g in (x, sc))
(* グラフの全てのノードが次数n未満か判定 *)
let is_ok g n = M.for_all (fun _ (l, _, _) -> List.length l < n) g
(* グラフからノードを除去.その変数と同じ命令内で使われてる変数のspillコストを増やす *)
let rem x g =
  M.map
    (fun (p,q,r) ->
      if List.mem x p then
	let sc = if S.mem x r then 10 else 0 in
	(List.filter (fun y -> x <> y) p, q+1+sc, r)
      else (p,q,r))
    (M.remove x g)
(* グラフの各ノードにspillのコストをつける *)
let score regenv prefer g =
  let prefer = M.map fst prefer in
  M.mapi
    (fun x (l, s, r) ->
      (l,
         (- List.length l) + (* 次数が大きいとspillコスト小 *)
	 (if M.mem x regenv && M.mem x prefer && List.exists is_reg (M.find x prefer) then
	   if List.mem (M.find x regenv) (M.find x prefer) then 8
	   else -8
	 else 0) + (* regenvとpreferが同じならコスト大,違うならコスト小 *)
	 3 * r, (* 参照回数が多いならコスト大 *)
       s)) (* 後で同じ命令内で使われてる変数がspillされたらコストをあげる *)
    g 
(* グラフのノードにspillのコストをふり,小さい順に消して,全ての次数をn未満にする *)
let spill regenv prefer g n =
  let g1 = score regenv prefer g in
  let rec spill' g s =
    if is_ok g n then (g, s) else
    try
      let (m,sc) = take_min g in
      let (l,_,_) = M.find m g1 in 
      spill' (rem m g) ((m,(l,sc))::s) 
    with Not_found -> (g, s) in
  let (g2, s) = spill' g1 [] in
  let g3 = M.map (fun (l,sc,_) -> (l, sc)) g2 in
  (* グラフに戻しても全ての次数がn未満で保たれるノードを戻す *)
  let addn x l sc g =
    let l' = List.filter (fun t -> M.mem t g) l in
    List.fold_left
      (fun m y ->
	let (p,q) = M.find y m in
	if List.length p + 1 >= n then failwith "fail"
	else M.add y (x::p,q) m)
      (M.add x (l', sc) g)
      l' in
  let (g4, s') = 
    List.fold_left
      (fun (g', s'') (y, (l, sc)) ->
	try (addn y l sc g', s'') with (Failure "fail") -> (g', s''@[(y,l)]))
      (g3, [])
      s in
  let g'' =
    List.map (fun (x, (l,_)) -> (x, l))
      (List.sort (fun (_, (_, sc)) (_, (_, sc')) -> compare sc' sc)
	 (M.fold (fun x y l -> (x,y)::l) g4 [])) in
  (g'', s')



(* グラフの彩色 *)
let able c l g =
  List.for_all
    (fun x -> match List.assoc x g with
              | (Colored(d),_) when c = d -> false
	      | _ -> true)
    l 
let color cs regenv prefer g s =
  (* まずregenv,preferにしたがって情報を付加 *)
  let g1 =
    List.map
      (fun (x, l) ->
	let y =
	  if is_reg x then Colored(x) else
	  try Colored(M.find x regenv) with Not_found ->
	    let (k, h) =
	      try
		let (p,s) = M.find x prefer in
		(List.filter (fun x -> List.mem_assoc x g || List.mem x cs) p,
		 List.filter (fun x -> List.mem_assoc x g || List.mem x cs) s)
	      with Not_found -> ([],[]) in
	    Prefer (k, h) in
	(x, (y, l)))
      g in
  let s1 =
    List.map
      (fun (x, l) ->
	let y =
	  if is_reg x then Colored(x) else
	  let (k, h) =
	    try
	      let (p,s) = M.find x prefer in
	      try 
	      (M.find x regenv::List.filter (fun x -> List.mem_assoc x g || List.mem x cs) p, List.filter (fun x -> List.mem_assoc x g || List.mem x cs) s)
	      with Not_found -> (List.filter (fun x -> List.mem_assoc x g || List.mem x cs) p, List.filter (fun x -> List.mem_assoc x g || List.mem x cs) s)
	    with Not_found -> try ([M.find x regenv], [])
	    with Not_found -> ([], []) in
	  Prefer (k, h) in
	(x, (y, l)))
      s in

  (* 他の変数の塗りたい色に塗るのはなるべく避ける *)
  let add_hate x l g =
    List.map
      (fun (x', p) ->
	if x = x' then
	  match p with
	  | (Prefer(p,s),k) -> (x', (Prefer(p, l@s),k))
	  | _ -> (x', p)
	else (x', p))
      g in
  let add_hates g (_, (y,l)) =
    match y with
    | Colored(c) -> List.fold_left (fun g z -> add_hate z [c] g) g l
    | Prefer(p,_) -> List.fold_left (fun g z -> add_hate z p g) g l in
  let g2 = List.fold_left add_hates (List.fold_left add_hates g1 g1) s1 in
  let s2 = List.fold_left add_hates s1 s1 in

  (* グラフを上書き.存在しなかったら追加. *)
  let rewrite x l g =
    if List.mem_assoc x g then
      List.map (fun (x', l') -> if x = x' then (x, l) else (x', l')) g
    else
      g@[(x, l)] in

  (* ノードに対し彩色する補助関数 *)
  (* 1次版 *)
  let col1 g (x, z) =
    match z with
    | (Prefer (p, s), l) ->
        let (p',q') = List.partition (fun x -> is_reg x) p in
	(* preferのうち一番多いやつに塗ろうとする *)
	let rec add x r = function
	  | [] -> (1,x)::r
	  | (n,y)::ys when x = y -> (n+1,y)::r@ys
	  | y::ys -> add x (y::r) ys in
	let rec count r = function
	  | [] -> r
	  | x::xs -> count (add x [] r) xs in
	let p'' = List.map snd (List.sort (fun x y -> compare y x) (count [] p')) in
	let s' =
	  List.map (fun x -> if is_reg x then x else match List.assoc x g with
	  | (Colored c,_) -> c | _ -> x) s in
	let (p2, p1) = List.partition (fun x -> List.mem x s') p'' in
	let y =
	  try Colored(List.find (fun c -> able c l g) (p1@p2))
	  with Not_found -> Prefer (q', s') in
        rewrite x (y, l) g
    | _ -> rewrite x z g in

  (* 2次版 *)
  let col2 g (x, y) =
    match y with
    | (Prefer (p,s), l) ->
        let q' = List.fold_left
	    (fun q' x ->
	      try
		match List.assoc x g with
		| (Colored(c),_) -> c::q'
		| _ -> q'
	      with Not_found -> q')
	    []
  	    p in
	let s' =
	  List.map (fun x -> if is_reg x then x else try match List.assoc x g with
	  | (Colored c,_) -> c | _ -> x with Not_found -> x) s in
	let (q2, q1) = List.partition (fun x -> List.mem x s') q' in
	let (cs2, cs1) = List.partition (fun x -> List.mem x s') cs in
        rewrite x (Colored(List.find (fun c -> able c l g) (q1@q2@cs1@cs2)), l) g
    | _ -> rewrite x y g in

  (* グラフをpreferにしたがって彩色 *)
  let g3 = List.fold_left col1 g2 g2 in
  let g4 = List.fold_left col2 g3 g3 in

  (* spillされた変数のうち,実は彩色できるものを彩色.残りは後で適当に塗る *)
  let el = function
    | (Prefer (p, h), l) -> (Prefer (List.filter is_reg p, List.filter is_reg h), l)
    | c -> c in

  let g5 = List.fold_left col1 g4 s2 in
  let g6 =
    List.fold_left
      (fun g5 (x,y) -> (try col2 g5 (x,y) with Not_found -> rewrite x (el y) g5))
      g5
      g5 in
  M.add_list (List.map (fun (x,y) -> (x, fst y)) g6) M.empty
