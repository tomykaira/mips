(* A正規化を行うモジュール *)

type t = (* let列 *)
  | Let of (Id.t * Type.t) * exp * t
  | LetRec of fundef * t
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Ans of exp
and exp = (* 式 *)
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
  | Var of Id.t
  | App of Id.t * Id.t list
  | Tuple of Id.t list
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.t
  | ExtFunApp of Id.t * Id.t list
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let rec concat e1 xt e2 =
  match e1 with
  | Let(yt, e3, e4) -> Let(yt, e3, concat e4 xt e2)
  | LetRec(fundefs, e) -> LetRec(fundefs, concat e xt e2)
  | LetTuple(yts, z, e) -> LetTuple(yts, z, concat e xt e2)
  | Ans(exp) -> Let(xt, exp, e2)

let rec fv = function (* 式に出現する（自由な）変数 *)
  | Let((x, t), exp, e) -> S.union (fv' exp) (S.remove x (fv e))
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) ->
      let zs = S.diff (fv e1) (S.of_list (List.map fst yts)) in
      S.diff (S.union zs (fv e2)) (S.singleton x)
  | LetTuple(xs, y, e) -> S.add y (S.diff (fv e) (S.of_list (List.map fst xs)))
  | Ans(exp) -> fv' exp
and fv' = function
  | Unit | Int(_) | Float(_) | ExtArray(_) -> S.empty
  | Neg(x) | FNeg(x) | Sll(x, _) | Sra(x, _) -> S.singleton x
  | Add(x, y) | Sub(x, y) | Mul(x, y) | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) | Get(x, y) -> S.of_list [x; y]
  | IfEq(x, y, e1, e2) | IfLE(x, y, e1, e2) | IfLT(x, y, e1, e2) -> S.add x (S.add y (S.union (fv e1) (fv e2)))
  | Var(x) -> S.singleton x
  | App(x, ys) -> S.of_list (x :: ys)
  | Tuple(xs) | ExtFunApp(_, xs) -> S.of_list xs
  | Put(x, y, z) -> S.of_list [x; y; z]



(* ネストしたletの簡約 *)
let rec f = function 
  | KNormal.Let(xt, e1, e2) -> concat (f e1) xt (f e2)
  | KNormal.LetRec({ KNormal.name = xt; KNormal.args = yts; KNormal.body = e1 }, e2) ->
      LetRec({ name = xt; args = yts; body = f e1 }, f e2)
  | KNormal.LetTuple(xts, y, e) -> LetTuple(xts, y, f e)
  | e -> Ans(f' e)
and f' = function
  | KNormal.Unit -> Unit
  | KNormal.Int(i) -> Int(i)
  | KNormal.Float(d) -> Float(d)
  | KNormal.Neg(x) -> Neg(x)
  | KNormal.Add(x, y) -> Add(x, y)
  | KNormal.Sub(x, y) -> Sub(x, y)
  | KNormal.Mul(x, y) -> Mul(x, y)
  | KNormal.Sll(x, y) -> Sll(x, y)
  | KNormal.Sra(x, y) -> Sra(x, y)
  | KNormal.FNeg(x) -> FNeg(x)
  | KNormal.FAdd(x, y) -> FAdd(x, y)
  | KNormal.FSub(x, y) -> FSub(x, y)
  | KNormal.FMul(x, y) -> FMul(x, y)
  | KNormal.FDiv(x, y) -> FDiv(x, y)
  | KNormal.IfEq(x, y, e1, e2) -> IfEq(x, y, f e1, f e2)
  | KNormal.IfLE(x, y, e1, e2) -> IfLE(x, y, f e1, f e2)
  | KNormal.IfLT(x, y, e1, e2) -> IfLT(x, y, f e1, f e2)
  | KNormal.Var(x) -> Var(x)
  | KNormal.App(x, ys) -> App(x, ys)
  | KNormal.Tuple(xs) -> Tuple(xs)
  | KNormal.Get(x, y) -> Get(x, y)
  | KNormal.Put(x, y, z) -> Put(x, y, z)
  | KNormal.ExtArray(x) -> ExtArray(x)
  | KNormal.ExtFunApp(x, ys) -> ExtFunApp(x, ys)
  | _ -> assert false


(******************************************************************)
(* デバッグ用関数. tを出力. nは深さ. *)
let rec ind m = if m <= 0 then ()
                else (Format.eprintf "  "; ind (m-1))
let rec dbprint n t =
  ind n;
  match t with
  | Let ((a, t), b, c) ->
      Format.eprintf "let (%s:%s) =%!" a (Type.show t);
      dbprint' (n+1) b; Format.eprintf " in@."; dbprint n c
  | LetRec (f, a) ->
     Format.eprintf "let rec (%s:%s) %s =%!" (fst f.name) (Type.show (snd f.name)) (String.concat " " (List.map (fun (x,y) -> "(" ^ x ^ ":" ^ Type.show y ^ ")" ) f.args));
     dbprint (n+1) f.body; ind n; Format.eprintf "in\n%!" ; dbprint (n+1) a
  | LetTuple (l, a, b) ->
   Format.eprintf "let (%s) = %s in\n%!" (String.concat "," (List.map (fun (x,y) -> "(" ^ x ^ ":" ^ Type.show y ^ ")") l)) a;
   dbprint (n+1) b
  | Ans(exp) -> dbprint' n exp; Format.eprintf "@."
and dbprint' n = function
  | Unit -> Format.eprintf "Unit%!"
  | Int a -> Format.eprintf "Int %d%!" a
  | Float a-> Format.eprintf "Float %f%!" a
  | Neg a -> Format.eprintf "Neg %s%!" a
  | Add (a, b) -> Format.eprintf "Add %s %s%!" a b
  | Sub (a, b) -> Format.eprintf "Sub %s %s%!" a b
  | Mul (a, b) -> Format.eprintf "Mul %s %s%!" a b
  | Sll (a, b) -> Format.eprintf "Sll %s %d%!" a b
  | Sra (a, b) -> Format.eprintf "Sra %s %d%!" a b
  | FNeg a -> Format.eprintf "FNeg %s%!" a
  | FAdd (a, b) -> Format.eprintf "FAdd %s %s%!" a b
  | FSub (a, b) -> Format.eprintf "FSub %s %s%!" a b
  | FMul (a, b) -> Format.eprintf "FMul %s %s%!" a b
  | FDiv (a, b) -> Format.eprintf "FDiv %s %s%!" a b
  | IfEq (a, b, p, q) ->
      Format.eprintf "@."; ind n;
      Format.eprintf "If %s = %s Then@." a b;
      dbprint (n+1) p; ind n; Format.eprintf "Else\n%!"; dbprint (n+1) q;
      ind (n-1)
  | IfLE (a, b, p, q) -> 
      Format.eprintf "@."; ind n;
      Format.eprintf "If %s <= %s Then@." a b;
      dbprint (n+1) p; ind n; Format.eprintf "Else\n%!"; dbprint (n+1) q;
      ind (n-1)
  | IfLT (a, b, p, q) ->
      Format.eprintf "@."; ind n;
      Format.eprintf "If %s < %s Then@." a b;
      dbprint (n+1) p; ind n; Format.eprintf "Else\n%!"; dbprint (n+1) q;
      ind (n-1)
  | Var a -> Format.eprintf "Var %s%!" a
  | App (a, l) -> Format.eprintf "App %s to %s%!" a (String.concat " " l)
  | Tuple l -> Format.eprintf "Tuple (%s)%!" (String.concat " , " l)
  | Get (a, b) -> Format.eprintf "Get %s %s \n%!" a b
  | Put (a, b, c) -> Format.eprintf "Put %s %s %s\n%!" a b c 
  | ExtArray a -> Format.eprintf "ExtArray %s\n%!" a
  | ExtFunApp (a, l) -> Format.eprintf "ExtFunApp %s to %s\n%!" a (String.concat " " l)

