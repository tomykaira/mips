(*pp deriving *)


type closure = { entry : Id.l; actual_fv : Id.t list }
    deriving (Show)
type t = (* クロージャ変換後の式 *)
  | Let of (Id.t * Type.t) * exp * t
  | MakeCls of (Id.t * Type.t) * closure * t
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | LetList of (Syntax.list_matcher * Type.t) * Id.t * t
  | Ans of exp
and exp = 
  | Unit
  | Int of int
  | Float of float
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | Mul of Id.t * Id.t
  | Sll of Id.t * int
  | Sra of Id.t * int
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | IfEq of Id.t * Id.t * t * t
  | IfLE of Id.t * Id.t * t * t
  | IfLT of Id.t * Id.t * t * t
  | IfNil of Id.t * t * t
  | Var of Id.t
  | AppCls of Id.t * Id.t list
  | AppDir of Id.l * Id.t list
  | Tuple of Id.t list
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | GetTuple of Id.t * Id.t 
  | PutTuple of Id.t * Id.t * Id.t list
  | ExtArray of Id.l
  | Nil
  | Cons of Id.t * Id.t
      deriving (Show)
type global_array = { gname : Id.l * Type.t;
		      length : int }
    deriving (Show)
type global_closure = { cname : Id.l * Type.t }
type fundef = { name : Id.l * Type.t;
		args : (Id.t * Type.t) list;
		formal_fv : (Id.t * Type.t) list;
		body : t }
    deriving (Show)
type prog = Prog of global_array list * fundef list * t
    deriving (Show)

let rec fv = function
  | Let((x, _), exp, e) -> S.union (fv' exp) (S.remove x (fv e))
  | LetTuple(xts, y, e) -> S.add y (S.diff (fv e) (S.of_list (List.map fst xts)))
  | MakeCls((x, _), { entry = _; actual_fv = ys }, e) -> S.remove x (S.union (S.of_list ys) (fv e))
  | LetList((matcher, _), y, e) ->
    S.add y (S.diff (fv e) (S.of_list (Syntax.matcher_variables matcher)))
  | Ans(exp) -> fv' exp
and fv' = function
  | Unit | Nil | Int(_) | Float(_) | ExtArray(_) -> S.empty
  | Neg(x) | FNeg(x) | Sll(x, _) | Sra(x, _) -> S.singleton x
  | Add(x, y) | Sub(x, y) | Mul(x, y) | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) | Get(x, y) | GetTuple(x, y) | Cons(x, y) -> S.of_list [x; y]
  | IfEq(x, y, e1, e2)| IfLE(x, y, e1, e2) | IfLT (x, y, e1, e2) -> S.add x (S.add y (S.union (fv e1) (fv e2)))
  | IfNil(x, e1, e2) -> S.add x (S.union (fv e1) (fv e2))
  | Var(x) -> S.singleton x
  | AppCls(x, ys) -> S.of_list (x :: ys)
  | AppDir(_, xs) | Tuple(xs) -> S.of_list xs
  | Put(x, y, z) -> S.of_list [x; y; z]
  | PutTuple(x,y,z) -> S.of_list (x::y::z)

let rec fv_label x = function
  | Let(_, exp, e) ->  (fv_label' x exp) || (fv_label x e)
  | LetTuple(_, _, e) | MakeCls(_, _, e) | LetList(_, _, e) -> fv_label x e
  | Ans(exp) -> fv_label' x exp
and fv_label' x = function
  | IfEq(_, _, e1, e2)| IfLE(_, _, e1, e2) | IfLT (_, _, e1, e2) | IfNil(_, e1, e2) ->
      (fv_label x e1) || (fv_label x e2)
  | ExtArray(Id.L(l)) when l = x -> true
  | _ -> false

let rec fv_dir x = function
  | Let(_, exp, e) -> (fv_dir' x exp) || (fv_dir x e)
  | LetTuple(_, _, e) | MakeCls(_, _, e) | LetList(_, _, e) -> fv_dir x e
  | Ans(exp) -> fv_dir' x exp
and fv_dir' x = function
  | IfEq(_, _, e1, e2)| IfLE(_, _, e1, e2) | IfLT (_, _, e1, e2) | IfNil(_, e1, e2) ->
      (fv_dir x e1) || (fv_dir x e2)
  | AppDir(Id.L(l),_) when x = l -> true
  | _ -> false


let rec concat e1 xt e2 =
  match e1 with
  | Let(yt, e3, e4) -> Let(yt, e3, concat e4 xt e2)
  | MakeCls(yt, closure, e) -> MakeCls(yt, closure, concat e xt e2)
  | LetTuple(yts, z, e) -> LetTuple(yts, z, concat e xt e2)
  | LetList(yt, z, e) -> LetList(yt, z, concat e xt e2)
  | Ans(exp) -> Let(xt, exp, e2)


let globals : global_array list ref = ref [] (* グローバル配列の集合 *)
let glbcls : global_closure list ref = ref [] (* グローバルクロージャの集合 *)
let toplevel : fundef list ref = ref [] (* トップレベル関数の集合 *)


(* gの中で使う補助関数1 *)
let rec memg x =
  List.exists (fun y -> fst (y.gname) = Id.L(x)) !globals ||
  List.exists (fun y -> fst (y.cname) = Id.L(x)) !glbcls
let rec findg x =
  try snd ((List.find (fun y -> fst (y.gname) = Id.L(x)) !globals).gname) with
    Not_found -> snd ((List.find (fun y -> fst (y.cname) = Id.L(x)) !glbcls).cname)

(* gの中で使う補助関数2 *)
let rec gl_args l =
  match l with
  | [] -> [], (fun t -> t)
  | x::xs ->
      let (xs', load) = gl_args xs in
      if memg x then
	let x' = Id.genid x in
	(x'::xs', (fun t -> Let((x', findg x), ExtArray(Id.L(x)), load t)))
      else (x::xs', load)
      

(* クロージャ変換ルーチン本体 *)
let rec g env known const top = function
  | ANormal.Let((x, t), exp, e) ->
      let exp' = g' env known const exp in
      let const' = match exp' with
      | Ans(Int(i)) -> M.add x i const
      | _ -> const in
      let gu = (Id.genid "unit", Type.Unit) in
      let rec add_global = function
	| Let((x', t'), exp, e) ->
	    let (exp'', env', xt') = add_global e in
	    (Let((x', t'), exp, exp''), M.add x' t' env', xt')
	| Ans(AppDir(Id.L("min_caml_create_array"), [y;z])) when top && M.mem y const ->
	    globals := { gname = (Id.L(x), t); length = M.find y const }::!globals;
	    let x' = Id.genid x in
	    (Let((x', t), ExtArray(Id.L(x)),
		 Ans(AppDir(Id.L("min_caml_array_init"), [x';y;z]))), env, gu)
	| Ans(AppDir(Id.L("min_caml_create_float_array"), [y;z])) when top && M.mem y const ->
	    globals := { gname = (Id.L(x), t); length = M.find y const }::!globals;
	    let x' = Id.genid x in
	    (Let((x', t), ExtArray(Id.L(x)),
		 Ans(AppDir(Id.L("min_caml_float_array_init"), [x';y;z]))), env, gu)
	| Ans(AppDir(Id.L("min_caml_create_tuple_array"), [y;z])) when top && M.mem y const ->
	    globals := { gname = (Id.L(x), t); length = M.find y const }::!globals;
	    let x' = Id.genid x in
	    (Let((x', t), ExtArray(Id.L(x)),
		 Ans(AppDir(Id.L("min_caml_tuple_array_init"), [x';y;z]))), env, gu)
	| exp' -> (exp', M.add x t env, (x, t)) in
      let exp'', env', xt = add_global exp' in
      concat exp'' xt (g env' known const' top e)
  | ANormal.LetRec({ ANormal.name = (x, t); ANormal.args = yts; ANormal.body = e1 }, e2) -> (* 関数定義の場合 *)
      (* 関数定義let rec x y1 ... yn = e1 in e2の場合は、
	 xに自由変数がない(closureを介さずdirectに呼び出せる)
	 と仮定し、knownに追加してe1をクロージャ変換してみる *)
      let toplevel_backup = !toplevel in
      let globals_backup = !globals in
      let glbcls_backup = !glbcls in
      let env' = M.add x t env in
      let known' = S.add x known in
      let e1' = g (M.add_list yts env') known' const false e1 in
      (* 本当に自由変数がなかったか、変換結果e1'を確認する *)
      (* 注意: e1'にx自身が変数として出現する場合はclosureが必要! *)
      let zs = S.diff (fv e1') (S.of_list (List.map fst yts)) in
      let known', e1' =
	if S.is_empty zs then known', e1' else
	(* 駄目だったら状態を戻して、クロージャ変換をやり直す *)
	(toplevel := toplevel_backup;
	 globals := globals_backup;
	 glbcls := glbcls_backup;
	 (* トップレベルで宣言されている関数ならアドレスを静的に決定 *)
	 (if top then glbcls := { cname = (Id.L(x), t) } :: !glbcls);
	 let e1' = g (M.add_list yts env') known const false e1 in
	 known, e1') in
      let zs = S.elements (S.diff (fv e1') (S.add x (S.of_list (List.map fst yts)))) in (* 自由変数のリスト *)
      let zts = List.map (fun z -> (z, M.find z env')) zs in (* ここで自由変数zの型を引くために引数envが必要 *)
      (toplevel := { name = (Id.L(x), t); args = yts; formal_fv = zts; body = e1' } :: !toplevel); 
      let kn = S.mem x known' in
      let e2' = g env' known' const top e2 in
      let fvs = S.mem x (fv e2') in
      let fvl =
	List.fold_left
	  (fun fvl { name = _; args = _; formal_fv = _; body = e } -> fvl || (fv_label x e))
	  (fv_label x e2')
	  (!toplevel) in
      let fvd =
	List.fold_left
	  (fun fvd { name = _; args = _; formal_fv = _; body = e } -> fvd || (fv_dir x e))
	  (fv_dir x e2')
	  (!toplevel) in
      if fvl || (fvd && not (kn)) then
	(* e2'もしくはトップレベル関数内にxがクロージャのラベルとして出現していたらglobalsに追加 *)
	((globals := { gname = (Id.L(x), t); length = List.length zs + 1 } :: !globals);
         MakeCls((x, t), { entry = Id.L(x); actual_fv = zs }, e2'))
      else if fvs then
	(* e2'にxが変数として出現していたら削除しない *)
        MakeCls((x, t), { entry = Id.L(x); actual_fv = zs }, e2')
      else e2' (* 出現しなければMakeClsを削除 *)
  | ANormal.LetTuple(xts, y, e) ->
      LetTuple(xts, y, g (M.add_list xts env) known const top e)
  | ANormal.LetList((matcher, typ), y, e) ->
      LetList((matcher, typ), y, g (M.add_list_matcher matcher (ref (Some typ)) env) known const top e)
  | ANormal.Ans(exp) -> g' env known const exp
and g' env known const =  function 
  | ANormal.Unit -> Ans(Unit)
  | ANormal.Int(i) -> Ans(Int(i))
  | ANormal.Float(d) -> Ans(Float(d))
  | ANormal.Neg(x) -> Ans(Neg(x))
  | ANormal.Add(x, y) -> Ans(Add(x, y))
  | ANormal.Sub(x, y) -> Ans(Sub(x, y))
  | ANormal.Mul(x, y) -> Ans(Mul(x, y))
  | ANormal.Sll(x, y) -> Ans(Sll(x, y))
  | ANormal.Sra(x, y) -> Ans(Sra(x, y))
  | ANormal.FNeg(x) -> Ans(FNeg(x))
  | ANormal.FAdd(x, y) -> Ans(FAdd(x, y))
  | ANormal.FSub(x, y) -> Ans(FSub(x, y))
  | ANormal.FMul(x, y) -> Ans(FMul(x, y))
  | ANormal.FDiv(x, y) -> Ans(FDiv(x, y))
  | ANormal.IfEq(x, y, e1, e2) -> Ans(IfEq(x, y, g env known const false e1, g env known const false e2))
  | ANormal.IfLE(x, y, e1, e2) -> Ans(IfLE(x, y, g env known const false e1, g env known const false e2))
  | ANormal.IfLT(x, y, e1, e2) -> Ans(IfLT(x, y, g env known const false e1, g env known const false e2))
  | ANormal.IfNil(x, e1, e2) -> Ans(IfNil(x, g env known const false e1, g env known const false e2))
  | ANormal.Var(x) ->
      if memg x then
	let x' = Id.genid x in
	Let((x', findg x), ExtArray(Id.L(x)), Ans(Var(x')))
      else Ans(Var(x))
  | ANormal.App(x, ys) when S.mem x known -> (* 関数適用の場合 *)
      let (ys', load) = gl_args ys in
      load (Ans(AppDir(Id.L(x), ys')))
  | ANormal.App(f, xs) ->
      let (xs', load) = gl_args xs in
      if memg f then
	load (Ans(AppDir(Id.L(f), xs')))
      else load (Ans(AppCls(f, xs')))
  | ANormal.Tuple(xs) ->
      let (xs', load) = gl_args xs in
      load (Ans(Tuple(xs')))
  | ANormal.Get(x, y) ->
      if memg x then
	let x' = Id.genid x in
	Let((x', findg x), ExtArray(Id.L(x)), Ans(Get(x', y)))
      else Ans(Get(x, y))
  | ANormal.Put(x, y, z) ->
      let rec loadx loadz =
	if memg x then
	  let x' = Id.genid x in
	  Let((x', findg x), ExtArray(Id.L(x)), loadz x')
	else loadz x in
      let loadz x =
	if memg z then
	  let z' = Id.genid z in
	  Let((z', findg z), ExtArray(Id.L(z)), Ans(Put(x, y, z')))
	else Ans(Put(x, y, z)) in
      loadx loadz
  | ANormal.ExtArray(x) -> Ans(ExtArray(Id.L(x)))
  | ANormal.ExtFunApp(x, ys) ->
      let (ys', load) = gl_args ys in      
      load (Ans(AppDir(Id.L("min_caml_" ^ x), ys')))
  | ANormal.Nil -> Ans(Nil)
  | ANormal.Cons(x, y) ->
      if memg x then
	let x' = Id.genid x in
	Let((x', findg x), ExtArray(Id.L(x)), Ans(Cons(x', y)))
      else Ans(Cons(x, y))

let f e =
  Format.eprintf "making closures...@.";
  toplevel := [];
  globals := [];
  let e' = g M.empty S.empty M.empty true e in
  Prog(!globals, List.rev !toplevel, e')
