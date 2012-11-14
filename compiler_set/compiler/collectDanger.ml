open Closure

(* タプルを返す関数でタプルをスタックに置くと危険な関数を収集 *)

let danger = ref S.empty

(* 変数xがLetTuple (...) = x 以外の使い方をされてないか調べる *)
let rec read_only x = function
  | Let(_, exp, e) -> read_only' x exp && read_only x e
  | MakeCls((y,t), { actual_fv = ys }, e) ->
      List.for_all (fun z -> x <> z) ys && read_only x e
  | LetTuple(_,_,e) | LetList(_,_,e) -> read_only x e
  | Ans(exp) -> read_only' x exp
and read_only' x = function
  | Unit | Nil | Int(_) | Float(_) | ExtArray(_) | Neg(_) | FNeg(_) | Sll(_, _) | Sra(_, _) | Add(_, _) | Sub(_, _) | Mul(_, _) | FAdd(_, _) | FSub(_, _) | FMul(_, _) | FDiv(_, _) | Get(_, _) | GetTuple(_,_)  -> true
  | Var(y) | Put(_,_,y) | Cons(y,_) -> y <> x
  | IfEq(_, _, e1, e2)| IfLE(_, _, e1, e2) | IfLT (_, _, e1, e2) | IfNil(_, e1, e2) ->
         read_only x e1 && read_only x e2
  | AppCls(_, ys) | AppDir(_, ys) | Tuple(ys) | PutTuple(_,_,ys) -> List.for_all (fun y -> x <> y) ys

(* そのプログラムの末尾で呼び出されている関数をdangerに追加 *)
let rec call_leaf = function
  | Let(_, _, e)  | MakeCls(_,_,e) | LetTuple(_,_,e) | LetList(_,_,e)  -> call_leaf e
  | Ans(exp) -> call_leaf' exp
and call_leaf' = function
  | AppCls(x,_) | AppDir(Id.L(x),_) -> danger := S.add x !danger
  | IfEq(_, _, e1, e2)| IfLE(_, _, e1, e2) | IfLT (_, _, e1, e2) | IfNil(_, e1, e2) ->
         call_leaf e1; call_leaf e2
  | _ -> ()


(* 本体 *)
let rec g = function
  | Let((x,Type.Tuple(_)), exp, e) when not (read_only x e) ->
      call_leaf' exp; g' exp; g e
  | Let(_, exp, e) -> g' exp; g e
  | MakeCls(_,_,e) | LetTuple(_,_,e) | LetList(_,_,e) -> g e
  | Ans(exp) -> g' exp
and g' = function
  | IfEq(_, _, e1, e2) | IfLE(_, _, e1, e2) | IfLT(_, _, e1, e2) | IfNil(_, e1, e2) ->
      g e1; g e2
  | _ -> ()


(* 関数用 *)
let h { name = _; args = _; formal_fv = _; body = e } =
  call_leaf e;
  g e
  

let f (Prog(toplevel, e)) =
  Format.eprintf "collecting dangerous functions...@.";
  List.iter h toplevel;
  g e;
  Prog(toplevel, e)
