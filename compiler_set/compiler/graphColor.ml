open Asm

(* グラフ彩色によるレジスタ割り当て *)

(* ノードの状態を示すデータ型 *)
type info =
  | Colored of Id.t
  | Prefer of Id.t list * Id.t list

(* デバッグ用関数 *)
let print_env env =
  M.iter (fun x y -> Format.eprintf "%s:%s " x y) env;
  Format.eprintf "@."
let print_graph g =
  M.iter (fun x y -> Format.eprintf "%s : " x; List.iter (Format.eprintf "%s,") y; Format.eprintf "@.") g;
  Format.eprintf "@."
let print_colored_graph g =
  let q = function
    | Colored c -> c
    | Prefer (p,s) -> "p("^String.concat "," p ^ " // "^ String.concat "," s ^")" in
  M.iter (fun x (y,_) -> Format.eprintf "%s:%s " x (q y)) g;
  Format.eprintf "@."


(* グラフにノードを追加する関数 *)
let addn' x l m =
  try let k = M.find x m in M.add x (remove_and_uniq S.empty (l@k)) m
  with Not_found -> M.add x l m
let addn x l g =
  List.fold_left
    (fun r y -> addn' y [x] r)
    (addn' x l g)
    l 
(* 変数のリストから完全グラフを作る関数 *)
let perf l =
  List.fold_left
    (fun r y -> addn y (remove_and_uniq (S.singleton y) l) r)
    M.empty
    l 
(* 2つのグラフを合わせる関数 *)
let union g1 g2 = M.fold addn' g1 g2 
(* 命令列(分岐,関数呼び出しなし)に対し整数,浮動小数のグラフを構成 *)
let rec make' rr rf dest cont = function
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
      let (rr'', rf'') = make'' rr' rf' xt cont' exp in
      make' rr'' rf'' dest cont e
and make'' rr rf dest cont = function
  | IfEq(_,_,e1,e2) | IfLE(_,_,e1,e2) | IfLT(_,_,e1,e2) | IfFEq(_,_,e1,e2) | IfFLE(_,_,e1,e2) | IfFLT(_,_,e1,e2) ->
      let (rr1, rf1) = make' rr rf dest cont e1 in
      let (rr2, rf2) = make' rr rf dest cont e2 in
      (union rr1 rr2, union rf1 rf2)
  | exp -> (rr, rf)
let make e =
  (* まず,一番最初に生きている変数で完全グラフを作る *)
  let rr = perf (fv_int e) in
  let rf = perf (fv_float e) in
  make' rr rf ("%g0", Type.Unit) (Ans(Nop)) e
	

(* グラフのノードを次数の大きい順に消して,全ての次数をn未満にする *)
let take_max g =
  M.fold
    (fun x l (y,k) -> if List.length l > List.length k then (x,l) else (y,k))
    g
    (M.choose g)
let rem x g = M.map (List.filter (fun y -> x <> y)) (M.remove x g)
let spill g n = 
  let rec spill' g s =
    try
      let (m,ml) = take_max g in
      if List.length ml < n then (g, s)
      else spill' (rem m g) (m::s) 
    with Not_found -> (g, s) in
  let (g', s) = spill' g [] in
  (g', List.fold_left (fun r y -> M.add y (M.find y g) r) M.empty s)



(* グラフの彩色 *)

let able c l g =
  List.for_all
    (fun x -> match M.find x g with
              | (Colored(d),_) when c = d -> false
	      | _ -> true)
    l 
let color cs regenv prefer g s = 
  (* まずregenv,preferにしたがって情報を付加 *)
  let g1 =
    M.mapi
      (fun x l ->
	let y =
	  if is_reg x then Colored(x) else
	  try Colored (M.find x regenv)
	  with Not_found ->
	    let k =
	      try List.filter (fun x -> M.mem x g || List.mem x cs) (M.find x prefer)
	      with Not_found -> [] in
	    Prefer (k, []) in
	(y, l))
      g in
  let s1 =
    M.mapi
      (fun x l ->
	let y =
	  if is_reg x then Colored(x) else
	  let k =
	    try M.find x regenv::List.filter (fun x -> M.mem x g || List.mem x cs) (M.find x prefer)
	    with Not_found -> try [M.find x regenv]
	    with Not_found -> try (List.filter (fun x -> List.mem x allfregs) (M.find x prefer))
	    with Not_found -> [] in
	  Prefer (k,[]) in
	(y, l))
      s in

  (* 他の変数の塗りたい色に塗るのはなるべく避ける *)
  let add_hate x l g =
    try (match M.find x g with
    | (Prefer(p,s),k) -> M.add x (Prefer(p, l@s),k) g
    | _ -> g)
    with Not_found -> g in
  let add_hates x (y,l) g =
    match y with
	| Colored(c) -> List.fold_left (fun g z -> add_hate z [c] g) g l
	| Prefer(p,_) -> List.fold_left (fun g z -> add_hate z p g) g l in
  let g2 = M.fold add_hates s1 (M.fold add_hates g1 g1) in
  let s2 = M.fold add_hates s1 s1 in

  (* ノードに対し彩色する補助関数 *)
  (* 1次版 *)
  let col1 x y g =
    match y with
    | (Prefer (p, s), l) ->
        let (p',q') = List.partition (fun x -> is_reg x) p in
	let rec add x r = function
	  | [] -> (1,x)::r
	  | (n,y)::ys when x = y -> (n+1,y)::r@ys
	  | y::ys -> add x (y::r) ys in
	let rec count r = function
	  | [] -> r
	  | x::xs -> count (add x [] r) xs in
	let p'' = List.map snd (List.sort (fun x y -> compare y x) (count [] p')) in
	let (p2, p1) = List.partition (fun x -> List.mem x s) p'' in
	let y =
	  try Colored(List.find (fun c -> able c l g) (p1@p2))
	  with Not_found -> Prefer (q', s) in
        M.add x (y, l) g
    | _ -> M.add x y g in

  (* 2次版 *)
  let col2 x y g =
    match y with
    | (Prefer (p,s), l) ->
        let q' = List.fold_left
	            (fun q' x -> match M.find x g with
	                         | (Colored(c),_) -> c::q'
		                 | _ -> q')
	            []
  	            p in
	let (q2, q1) = List.partition (fun x -> List.mem x s) q' in
	let (cs2, cs1) = List.partition (fun x -> List.mem x s) cs in
        M.add x (Colored(List.find (fun c -> able c l g) (q1@q2@cs1@cs2)), l) g
    | (Colored _, _) -> M.add x y g in

  (* グラフをpreferにしたがって彩色 *)
  let g3 = M.fold col1 g2 g2 in
  let g4 = M.fold col2 g3 g3 in

  (* spillされた変数のうち,実は彩色できるものを彩色.残りは後で適当に塗る。 *)
  let el = function
    | (Prefer (p, s), l) -> (Prefer (List.filter is_reg p, s), l)
    | c -> c in
  let g5 = M.fold col1 s2 g4 in
  let g6 =
    M.fold
      (fun x y g5 -> (try col2 x y g5 with Not_found -> M.add x (el y) g5))
      g5
      g5 in
  M.map fst g6



