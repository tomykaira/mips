(*pp deriving *)

(* give names to intermediate values (K-normalization) *)

(* デバッグ用。trueならKnormal.tを出力 *)
let debug = ref false

type t = (* K正規化後の式 *)
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
  | IfEq of Id.t * Id.t * t * t (* 比較 + 分岐 *)
  | IfLE of Id.t * Id.t * t * t (* 比較 + 分岐 *)
  | IfLT of Id.t * Id.t * t * t (* 比較 + 分岐 *)
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of Id.t * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.t
  | ExtFunApp of Id.t * Id.t list
  | Nil
  | Cons of Id.t * Id.t
  | LetList of (Syntax.list_matcher * Type.t) * Id.t * t
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }
    deriving (Show)

let rec fv = function (* 式に出現する（自由な）変数 *)
  | Unit | Nil | Int(_) | Float(_) | ExtArray(_) -> S.empty
  | Neg(x) | FNeg(x) | Sll(x, _) | Sra(x, _) -> S.singleton x
  | Add(x, y) | Sub(x, y) | Mul(x, y) | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) | Get(x, y) | Cons(x, y) -> S.of_list [x; y]
  | IfEq(x, y, e1, e2) | IfLE(x, y, e1, e2) | IfLT(x, y, e1, e2) -> S.add x (S.add y (S.union (fv e1) (fv e2)))
  | Let((x, t), e1, e2) -> S.union (fv e1) (S.remove x (fv e2))
  | Var(x) -> S.singleton x
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) ->
      let zs = S.diff (fv e1) (S.of_list (List.map fst yts)) in
      S.diff (S.union zs (fv e2)) (S.singleton x)
  | App(x, ys) -> S.of_list (x :: ys)
  | Tuple(xs) | ExtFunApp(_, xs) -> S.of_list xs
  | Put(x, y, z) -> S.of_list [x; y; z]
  | LetTuple(xs, y, e) -> S.add y (S.diff (fv e) (S.of_list (List.map fst xs)))
  | LetList((matcher, _), y, e) ->
    S.add y (S.diff (fv e) (S.of_list (Syntax.matcher_variables matcher)))
  

let insert_let (e, t) k = (* letを挿入する補助関数 *)
  match e with
  | Var(x) -> k x
  | _ ->
      let x = Id.gentmp t in
      let e', t' = k x in
      Let((x, t), e, e'), t'

let rec g env = function (* K正規化ルーチン本体 *)
  | Syntax.Unit -> Unit, Type.Unit
  | Syntax.Bool(b) -> Int(if b then 1 else 0), Type.Int (* 論理値true, falseを整数1, 0に変換 *)
  | Syntax.Int(i) -> Int(i), Type.Int
  | Syntax.Float(d) -> Float(d), Type.Float
  | Syntax.Not(e) -> g env (Syntax.If(e, Syntax.Bool(false), Syntax.Bool(true)))
  | Syntax.Neg(e) ->
      insert_let (g env e)
	(fun x -> Neg(x), Type.Int)
  | Syntax.Add(e1, e2) -> (* 足し算のK正規化 *)
      insert_let (g env e1)
	(fun x -> insert_let (g env e2)
	    (fun y -> Add(x, y), Type.Int))
  | Syntax.Sub(e1, e2) ->
      insert_let (g env e1)
	(fun x -> insert_let (g env e2)
	    (fun y -> Sub(x, y), Type.Int))
  | Syntax.Mul(e1, e2) ->
      insert_let (g env e1)
	(fun x -> insert_let (g env e2)
	    (fun y -> Mul(x, y), Type.Int))
  | Syntax.Sll(e1, i) ->
      insert_let (g env e1)
	(fun x -> Sll(x, i), Type.Int)
  | Syntax.Sra(e1, i) ->
      insert_let (g env e1)
	(fun x -> Sra(x, i), Type.Int)
  | Syntax.FNeg(e) ->
      insert_let (g env e)
	(fun x -> FNeg(x), Type.Float)
  | Syntax.FAdd(e1, e2) ->
      insert_let (g env e1)
	(fun x -> insert_let (g env e2)
	    (fun y -> FAdd(x, y), Type.Float))
  | Syntax.FSub(e1, e2) ->
      insert_let (g env e1)
	(fun x -> insert_let (g env e2)
	    (fun y -> FSub(x, y), Type.Float))
  | Syntax.FMul(e1, e2) ->
      insert_let (g env e1)
	(fun x -> insert_let (g env e2)
	    (fun y -> FMul(x, y), Type.Float))
  | Syntax.FDiv(e1, e2) ->
      insert_let (g env e1)
	(fun x -> insert_let (g env e2)
	    (fun y -> FDiv(x, y), Type.Float))
  | Syntax.Eq _ | Syntax.LE _ | Syntax.LT _ as cmp ->
      g env (Syntax.If(cmp, Syntax.Bool(true), Syntax.Bool(false)))
  | Syntax.If(Syntax.Not(e1), e2, e3) -> g env (Syntax.If(e1, e3, e2)) (* notによる分岐を変換 *)
  | Syntax.If(Syntax.Eq(e1, e2), e3, e4) ->
      insert_let (g env e1)
	(fun x -> insert_let (g env e2)
	    (fun y ->
	      let e3', t3 = g env e3 in
	      let e4', t4 = g env e4 in
	      IfEq(x, y, e3', e4'), t3))
  | Syntax.If(Syntax.LE(e1, e2), e3, e4) ->
      insert_let (g env e1)
	(fun x -> insert_let (g env e2)
	    (fun y ->
	      let e3', t3 = g env e3 in
	      let e4', t4 = g env e4 in
	      IfLE(x, y, e3', e4'), t3))
  | Syntax.If(Syntax.LT(e1, e2), e3, e4) ->
      insert_let (g env e1)
	(fun x -> insert_let (g env e2)
	    (fun y ->
	      let e3', t3 = g env e3 in
	      let e4', t4 = g env e4 in
	      IfLT(x, y, e3', e4'), t3))
  | Syntax.If(e1, e2, e3) -> g env (Syntax.If(Syntax.Eq(e1, Syntax.Bool(false)), e3, e2)) (* 比較のない分岐を変換 *)
  | Syntax.Let((x, t), e1, e2) ->
      let e1', t1 = g env e1 in
      let e2', t2 = g (M.add x t env) e2 in
      Let((x, t), e1', e2'), t2
  | Syntax.Var(x) when M.mem x env -> Var(x), M.find x env
  | Syntax.Var(x) -> (* 外部配列の参照 *)
      (match M.find x !Typing.extenv with
      | Type.Array(_) as t -> ExtArray x, t
      | _ -> failwith (Printf.sprintf "external variable %s does not have an array type" x))
  | Syntax.LetRec({ Syntax.name = (x, t); Syntax.args = yts; Syntax.body = e1 }, e2) ->
      let env' = M.add x t env in
      let e2', t2 = g env' e2 in
      let e1', t1 = g (M.add_list yts env') e1 in
      LetRec({ name = (x, t); args = yts; body = e1' }, e2'), t2
  | Syntax.App(Syntax.Var(f), e2s) when not (M.mem f env) -> (* 外部関数の呼び出し *)
      (match M.find f !Typing.extenv with
      | Type.Fun(_, t) ->
	  let rec bind xs = function (* "xs" are identifiers for the arguments *)
	    | [] -> ExtFunApp(f, xs), t
	    | e2 :: e2s ->
		insert_let (g env e2)
		  (fun x -> bind (xs @ [x]) e2s) in
	  bind [] e2s (* left-to-right evaluation *)
      | _ -> assert false)
  | Syntax.App(e1, e2s) ->
      (match g env e1 with
      | _, Type.Fun(_, t) as g_e1 ->
	  insert_let g_e1
	    (fun f ->
	      let rec bind xs = function (* "xs" are identifiers for the arguments *)
		| [] -> App(f, xs), t
		| e2 :: e2s ->
		    insert_let (g env e2)
		      (fun x -> bind (xs @ [x]) e2s) in
	      bind [] e2s) (* left-to-right evaluation *)
      | _ -> assert false)
  | Syntax.Tuple(es) ->
      let rec bind xs ts = function (* "xs" and "ts" are identifiers and types for the elements *)
	| [] -> Tuple(xs), Type.Tuple(ts)
	| e :: es ->
	    let _, t as g_e = g env e in
	    insert_let g_e
	      (fun x -> bind (xs @ [x]) (ts @ [t]) es) in
      bind [] [] es
  | Syntax.LetTuple(xts, e1, e2) ->
      insert_let (g env e1)
	(fun y ->
	  let e2', t2 = g (M.add_list xts env) e2 in
	  LetTuple(xts, y, e2'), t2)
  | Syntax.Array(e1, e2) ->
      insert_let (g env e1)
	(fun x ->
	  let _, t2 as g_e2 = g env e2 in
	  insert_let g_e2
	    (fun y ->
	      let l =
		match t2 with
		| Type.Float -> "create_float_array"
		| _ -> "create_array" in
	      ExtFunApp(l, [x; y]), Type.Array(t2)))
  | Syntax.Get(e1, e2) ->
      (match g env e1 with
      |	_, Type.Array(t) as g_e1 ->
	  insert_let g_e1
	    (fun x -> insert_let (g env e2)
		(fun y -> Get(x, y), t))
      | _ -> assert false)
  | Syntax.Put(e1, e2, e3) ->
      insert_let (g env e1)
	(fun x -> insert_let (g env e2)
	    (fun y -> insert_let (g env e3)
		(fun z -> Put(x, y, z), Type.Unit)))
  | Syntax.Match(argument, cases) ->
    let rec take_while_patterns_and_guard cases =
      match cases with
	| [] -> failwith "match x with should end with VarPattern"
	| ((Syntax.VarPattern(_), body) as case) :: cases' -> ([], case)
	| ((Syntax.IntPattern(_), body) as case) :: cases' ->
	  let (int_cases, var_case) = take_while_patterns_and_guard cases' in
	  (case :: int_cases, var_case)
    in
    let int_pattern value body cont x =
      insert_let (g env (Syntax.Int(value)))
	(fun p1 ->
	  let (body', _) = g env body in
	  let (cont_body, cont_type) = cont x in
	  IfEq(x, p1, body', cont_body), cont_type) in
    let var_pattern id body x =
      g env (Syntax.Let((id, Type.Int), Syntax.Var(x), body)) (* Type is limited to Int *)
    in
    let (int_cases, var_case) = take_while_patterns_and_guard cases in
    let (Syntax.VarPattern(var), body) = var_case in
    let expanded_patterns = List.fold_right (fun (Syntax.IntPattern(value), body) cont -> int_pattern value body cont) int_cases (var_pattern var body) in
    insert_let (g env argument) expanded_patterns
  | Syntax.Nil -> Nil, Type.List(ref None)
  | Syntax.Cons(x, xs) ->
    let (_, x_typ) as g_x = (g env x) in
    insert_let g_x
      (fun x -> insert_let (g env xs)
	(fun xs -> Cons(x, xs), Type.List(ref (Some x_typ))))
  | Syntax.LetList((matcher, typ), e1, e2) ->
    match !typ with
      | Some(actual_typ) ->
	insert_let (g env e1)
	  (fun y ->
	    let e2', t2 = g (M.add_list_matcher matcher typ env) e2 in
	    LetList((matcher, actual_typ), y, e2'), t2)
      | None -> failwith "Typing failed to resolve LetList"


(******************************************************************)
(* デバッグ用関数. tを出力. nは深さ. *)
let rec ind m = if m <= 0 then ()
                else (Printf.eprintf "  "; ind (m-1))
let rec dbprint n t =
  ind n;
  match t with
  | Unit -> Printf.eprintf "Unit\n%!"
  | Int a -> Printf.eprintf "Int %d\n%!" a
  | Float a-> Printf.eprintf "Float %f\n%!" a
  | Neg a -> Printf.eprintf "Neg %s\n%!" a
  | Add (a, b) -> Printf.eprintf "Add %s %s\n%!" a b
  | Sub (a, b) -> Printf.eprintf "Sub %s %s\n%!" a b
  | Mul (a, b) -> Printf.eprintf "Mul %s %s\n%!" a b
  | Sll (a, b) -> Printf.eprintf "Sll %s %d\n%!" a b
  | Sra (a, b) -> Printf.eprintf "Sra %s %d\n%!" a b
  | FNeg a -> Printf.eprintf "FNeg %s\n%!" a
  | FAdd (a, b) -> Printf.eprintf "FAdd %s %s\n%!" a b
  | FSub (a, b) -> Printf.eprintf "FSub %s %s\n%!" a b
  | FMul (a, b) -> Printf.eprintf "FMul %s %s\n%!" a b
  | FDiv (a, b) -> Printf.eprintf "FDiv %s %s\n%!" a b
  | IfEq (a, b, p, q) -> Printf.eprintf "If %s = %s Then\n%!" a b;
                         dbprint (n+1) p; ind n; Printf.eprintf "Else\n%!"; dbprint (n+1) q
  | IfLE (a, b, p, q) -> Printf.eprintf "If %s <= %s Then\n%!" a b;
                         dbprint (n+1) p; ind n; Printf.eprintf "Else\n%!"; dbprint (n+1) q
  | IfLT (a, b, p, q) -> Printf.eprintf "If %s < %s Then\n%!" a b;
                         dbprint (n+1) p; ind n; Printf.eprintf "Else\n%!"; dbprint (n+1) q
  | Let ((a, t), b, c) -> Printf.eprintf "Let (%s:%s) =\n%!" a (Type.show t);
                          dbprint (n+1) b; ind n; Printf.eprintf "In\n%!"; dbprint (n+1) c
  | Var a -> Printf.eprintf "Var %s\n%!" a
  | LetRec (f, a) ->
     Printf.eprintf "LetRec (%s:%s) %s =\n%!" (fst f.name) (Type.show (snd f.name)) (String.concat " " (List.map (fun (x,y) -> "(" ^ x ^ ":" ^ Type.show y ^ ")" ) f.args));
     dbprint (n+1) f.body; ind n; Printf.eprintf "In\n%!" ; dbprint (n+1) a
  | App (a, l) -> Printf.eprintf "App %s to %s\n%!" a (String.concat " " l)
  | Tuple l -> Printf.eprintf "Tuple (%s)\n%!" (String.concat " , " l)
  | LetTuple (l, a, b) ->
   Printf.eprintf "Let (%s) = %s in\n%!" (String.concat "," (List.map (fun (x,y) -> "(" ^ x ^ ":" ^ Type.show y ^ ")") l)) a;
   dbprint (n+1) b
  | Get (a, b) -> Printf.eprintf "Get %s %s \n%!" a b
  | Put (a, b, c) -> Printf.eprintf "Put %s %s %s\n%!" a b c 
  | ExtArray a -> Printf.eprintf "ExtArray %s\n%!" a
  | ExtFunApp (a, l) -> Printf.eprintf "ExtFunApp %s to %s\n%!" a (String.concat " " l)
  | Nil -> Printf.eprintf "Nil\n%!"
  | Cons(x, xs) -> Printf.eprintf "Cons %s %s\n%!" x xs
  | LetList ((matcher, typ), a, b) ->
    Printf.eprintf "Let (%s : %s) = %s in\n%!" (Syntax.string_of_matcher matcher) (Type.show typ) a;
    dbprint (n+1) b


let f e = fst (g M.empty e)
