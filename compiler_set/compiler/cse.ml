open KNormal

(* env4はPutや外部関数呼び出しをする関数,env5はGetや外部関数呼び出しをする関数 *)
let env4 = ref [] 
let env5 = ref []
    
(* どのくらいの間使いまわすか *)
let keep = 32

(* リストの末尾をn個drop *)
let rec drop_to n l =
  match l with
  | [] -> []
  | _ when n <= 0 -> []
  | x::xs -> x::(drop_to (n-1) xs)

(* 2つのK正規形の式が(変数名を含めて)同じであるかどうか調べる関数 *)
let rec same_exp e1 e2 =
  match e1, e2 with
  | Unit, Unit -> true
  | Int(i), Int(j) when i = j -> true
  | Float(d), Float(e) when d = e -> true
  | Neg(x), Neg(y) | FNeg(x), FNeg(y)  | Var(x), Var(y) 
  | ExtArray(x), ExtArray(y) -> x = y
  | Sll(x,y), Sll(z,w) | Sra(x,y), Sra(z,w) when y = w -> x = z
  | Add(x,y), Add(z,w) | Mul(x,y), Mul(z,w) | FAdd(x,y), FAdd(z,w)
  | FMul(x,y), FMul(z,w) ->  (x = z && y = w) || (x = w && y = z)
  | Sub(x,y), Sub(z,w) | FSub(x,y), FSub(z,w) | FDiv(x,y), FDiv(z,w)
  | Get(x,y), Get(z,w)  -> x = z && y = w
  | IfEq(x,y,e1,e2) , IfEq(z,w,e3,e4) when (x = z && y = w) || (x = w && y = z)
    -> same_exp e1 e3 && same_exp e2 e4
  | IfLE(x,y,e1,e2) , IfLE(z,w,e3,e4) | IfLT(x,y,e1,e2) , IfLT(z,w,e3,e4)
      when x = z && y = w -> same_exp e1 e3 && same_exp e2 e4
  | Let((x,t), e1, e2), Let((y,s), e3, e4) ->
      t = s && same_exp e1 e3 && same_exp e2 (Beta.g (M.add y x M.empty) e4)
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2), LetRec({ name = (z, s); args = wts; body = e3 }, e4) ->
      t = s && same_fun yts e1 wts e3
  | LetTuple(xts, y, e1), LetTuple(zts, w, e2) ->
      y = w && same_fun xts e1 zts e2
  | Tuple(x) ,Tuple(y) -> x = y
  | Put(x, y, z) , Put(u, v, w) -> x = u && y = v && z = w
  | App(x,y), App(z,w) | ExtFunApp(x,y), ExtFunApp(z,w) -> x = z && y = w
  | _ -> false
(* 2つの関数が同じかどうか調べる関数 *)
and same_fun s1 e1 s2 e2 =
  try let (x1, t1) = List.split s1 in
      let (x2, t2) = List.split s2 in
      t1 = t2 && same_exp e1 (Beta.g (List.fold_right2 M.add x2 x1 M.empty) e2)
  with Invalid_argument _ -> false

(* 部分式をインデックスに変数を引く関数 *)
let finds x env = snd (List.find (fun (s,_) -> same_exp x s) env)
(* 関数(型つき引数のリストと本体の組)をインデックスに変数を引く関数 *)
let findf (l, e) env =
  snd (List.find (fun ((l',e'), _) -> same_fun l e l' e') env)


(* K正規形の式の中にPutや外部関数呼び出しがあるかどうかを返す関数 *)
let rec put_array = function
  | Put(_,_,_) | ExtFunApp(_,_) -> true
  | IfEq(_,_,e1,e2) | IfLE(_,_,e1,e2) | IfLT(_,_,e1,e2) | Let(_, e1, e2) ->
      put_array e1 || put_array e2
  | LetRec({ name = xt; args = _; body = e1 }, e2) ->
      (if put_array e1 then env4 := fst(xt)::!env4 else ());
      put_array e2
  | App(g, _) -> List.mem g !env4
  | _ -> false

(* K正規形の式の中にGetや外部関数呼び出しがあるかどうかを返す関数 *)
let rec get_array = function
  | Get(_,_) | ExtFunApp(_,_) -> true
  | IfEq(_,_,e1,e2) | IfLE(_,_,e1,e2) | IfLT(_,_,e1,e2) | Let(_, e1, e2) ->
      get_array e1 || get_array e2
  | LetRec({ name = xt; args = _; body = e1 }, e2) ->
      (if get_array e1 then env5 := fst(xt)::!env5 else ());
      get_array e2
  | App(g, _) -> List.mem g !env5
  | _ -> false


(* 共通部分式除去を行う関数.env1はGetをしない部分式,env2はGetをする部分式,
   env3は出現した関数定義.Putや外部関数呼び出しをする部分式かenv4の関数が出現したら
   env2を空にする。返り値の第1要素は何回代入が起きたか。 *)
let rec g env1 env2 env3 e =
  try 0, Var(finds e env1)
  with Not_found ->
  try 0, Var(finds e env2)
  with Not_found ->
    match e with
    | IfEq(x, y, e1, e2) ->
	let (a, e1') = g env1 env2 env3 e1 in
	let (b, e2') = g env1 env2 env3 e2 in
	(a+b, IfEq(x, y, e1', e2'))
    | IfLE(x, y, e1, e2) ->
	let (a, e1') = g env1 env2 env3 e1 in
	let (b, e2') = g env1 env2 env3 e2 in
	(a+b, IfLE(x, y, e1', e2'))
    | IfLT(x, y, e1, e2) ->
	let (a, e1') = g env1 env2 env3 e1 in
	let (b, e2') = g env1 env2 env3 e2 in
	(a+b, IfLT(x, y, e1', e2'))
    | Let((x,t), e1, e2) ->
	let (a, e1') = g env1 env2 env3 e1 in
	let q = keep - a - 1 in
	let (b, e2') =
   	  if put_array e1' then
	    g (drop_to q env1) [] (drop_to q env3) e2
	  else if get_array e1' then
	    g (drop_to q env1) ((e1',x)::(drop_to q env2)) (drop_to q env3) e2
	  else
	    g ((e1',x)::(drop_to q env1)) (drop_to q env2) (drop_to q env3) e2
	in (a+b+1, Let((x,t), e1', e2'))
    | LetRec({ name = xt; args = yts; body = e1 }, e2) ->
	(let x = keep-1 in
	 let (_, e1') = g env1 env2 env3 e1 in
	try let t = findf (yts, e1') env3 in
	    let (a, e2') = g (drop_to x env1) (drop_to x env2) (drop_to x env3) e2 in
	    (a+1, Let(xt, Var(t), e2'))
	with Not_found ->
	  (if put_array e1' then env4 := fst(xt)::!env4
	   else if get_array e1' then env5 := fst(xt)::!env5
	   else ());
	  let (a,e2')=g (drop_to x env1) (drop_to x env2) (((yts, e1'), fst xt)::(drop_to (keep-1) env3)) e2
	  in (a+1, LetRec({ name = xt; args = yts; body = e1' }, e2')))
    | LetTuple(l, y, e1) ->
	let n = List.length l in
	let x = keep - n in
	let (a, e1') = g (drop_to x env1) (drop_to x env2) (drop_to x env3) e1 in
        (a+n, LetTuple(l, y, e1'))
    | App(x,y) -> (100, App(x,y))
    | ExtFunApp(x,y) -> (100, ExtFunApp(x,y))
    | _ -> (0, e)
	 

let f e = Format.eprintf "eliminating common subexpressions...@.";
          env4 := []; env5 := []; snd(g [] [] [] e)
