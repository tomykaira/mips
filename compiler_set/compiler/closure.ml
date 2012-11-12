(*pp deriving *)

let danger = ref []

type closure = { entry : Id.l; actual_fv : Id.t list }
    deriving (Show)
type t = (* クロージャ変換後の式 *)
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
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | MakeCls of (Id.t * Type.t) * closure * t
  | AppCls of Id.t * Id.t list
  | AppDir of Id.l * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.l
  | Nil
  | Cons of Id.t * Id.t
  | LetList of (Syntax.list_matcher * Type.t) * Id.t * t
      deriving (Show)
type fundef = { name : Id.l * Type.t;
                args : (Id.t * Type.t) list;
                formal_fv : (Id.t * Type.t) list;
                body : t }
    deriving (Show)
type prog = Prog of fundef list * t
    deriving (Show)

let rec fv = function
  | Unit | Nil | Int(_) | Float(_) | ExtArray(_) -> S.empty
  | Neg(x) | FNeg(x) | Sll(x, _) | Sra(x, _) -> S.singleton x
  | Add(x, y) | Sub(x, y) | Mul(x, y) | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) | Get(x, y) | Cons(x, y) -> S.of_list [x; y]
  | IfEq(x, y, e1, e2)| IfLE(x, y, e1, e2) | IfLT (x, y, e1, e2) -> S.add x (S.add y (S.union (fv e1) (fv e2)))
  | IfNil(x, e1, e2) -> S.add x (S.union (fv e1) (fv e2))
  | Let((x, t), e1, e2) -> S.union (fv e1) (S.remove x (fv e2))
  | Var(x) -> S.singleton x
  | MakeCls((x, t), { entry = l; actual_fv = ys }, e) -> S.remove x (S.union (S.of_list ys) (fv e))
  | AppCls(x, ys) -> S.of_list (x :: ys)
  | AppDir(_, xs) | Tuple(xs) -> S.of_list xs
  | LetTuple(xts, y, e) -> S.add y (S.diff (fv e) (S.of_list (List.map fst xts)))
  | Put(x, y, z) -> S.of_list [x; y; z]
  | LetList((matcher, _), y, e) ->
    S.add y (S.diff (fv e) (S.of_list (Syntax.matcher_variables matcher)))

let rec read_only x = function
  | Unit | Nil | Int(_) | Float(_) | ExtArray(_) | Neg(_) | FNeg(_) | Sll(_, _) | Sra(_, _) | Add(_, _) | Sub(_, _) | Mul(_, _) | FAdd(_, _) | FSub(_, _) | FMul(_, _) | FDiv(_, _) | Get(_, _) -> true
  | Var(y) | Put(_,y,_) -> y <> x
  | Cons(y,z) -> x <> y && x <> z
  | IfEq(_, _, e1, e2)| IfLE(_, _, e1, e2) | IfLT (_, _, e1, e2) | IfNil(_, e1, e2) | Let(_, e1, e2) ->
         read_only x e1 && read_only x e2
  | MakeCls(_, { actual_fv = ys }, e) -> List.for_all (fun y -> x <> y) ys && read_only x e
  | AppCls(_, ys) | AppDir(_, ys) | Tuple(ys) -> List.for_all (fun y -> x <> y) ys
  | LetTuple(_,_,e) | LetList(_,_,e) -> read_only x e
let rec call_leaf = function
  | AppCls(x,_) | AppDir(Id.L(x),_) -> danger := x::!danger
  | IfEq(_, _, e1, e2)| IfLE(_, _, e1, e2) | IfLT (_, _, e1, e2) | IfNil(_, e1, e2)  ->
         call_leaf e1; call_leaf e2
  | LetTuple(_,_,e) | LetList(_,_,e) | MakeCls(_,_,e) | Let(_, _, e) -> call_leaf e
  | _ -> ()


let toplevel : fundef list ref = ref []

let rec g env known = function (* クロージャ変換ルーチン本体 *)
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
  | KNormal.IfEq(x, y, e1, e2) -> IfEq(x, y, g env known e1, g env known e2)
  | KNormal.IfLE(x, y, e1, e2) -> IfLE(x, y, g env known e1, g env known e2)
  | KNormal.IfLT(x, y, e1, e2) -> IfLT(x, y, g env known e1, g env known e2)
  | KNormal.IfNil(x, e1, e2) -> IfNil(x, g env known e1, g env known e2)
  | KNormal.Let((x, t), e1, e2) ->
    let e1' = g env known e1 in
    let e2' = g (M.add x t env) known e2 in
    (match t with
      | Type.Tuple(_) when not (read_only x e2') -> call_leaf e1'
      | _ -> ());
    Let((x, t), e1', e2')
  | KNormal.Var(x) -> Var(x)
  | KNormal.LetRec({ KNormal.name = (x, t); KNormal.args = yts; KNormal.body = e1 }, e2) -> (* 関数定義の場合 *)
    (* 関数定義let rec x y1 ... yn = e1 in e2の場合は、
       xに自由変数がない(closureを介さずdirectに呼び出せる)
       と仮定し、knownに追加してe1をクロージャ変換してみる *)
    let toplevel_backup = !toplevel in
    let env' = M.add x t env in
    let known' = S.add x known in
    let e1' = g (M.add_list yts env') known' e1 in
    (* 本当に自由変数がなかったか、変換結果e1'を確認する *)
    (* 注意: e1'にx自身が変数として出現する場合はclosureが必要! *)
    let zs = S.diff (fv e1') (S.of_list (List.map fst yts)) in
    let known', e1' =
      if S.is_empty zs then known', e1' else
        (* 駄目だったら状態(toplevelの値)を戻して、クロージャ変換をやり直す *)
        (toplevel := toplevel_backup;
         let e1' = g (M.add_list yts env') known e1 in
         known, e1') in
    (match t with
      | Type.Fun(_,Type.Tuple(_)) -> call_leaf e1'
      | _ -> ());
    let zs = S.elements (S.diff (fv e1') (S.add x (S.of_list (List.map fst yts)))) in (* 自由変数のリスト *)
    let zts = List.map (fun z -> (z, M.find z env')) zs in (* ここで自由変数zの型を引くために引数envが必要 *)
    toplevel := { name = (Id.L(x), t); args = yts; formal_fv = zts; body = e1' } :: !toplevel; (* トップレベル関数を追加 *)
    let e2' = g env' known' e2 in
    if S.mem x (fv e2') then (* xが変数としてe2'に出現するか *)
      MakeCls((x, t), { entry = Id.L(x); actual_fv = zs }, e2') (* 出現していたら削除しない *)
    else
      e2' (* 出現しなければMakeClsを削除 *)
  | KNormal.App(x, ys) when S.mem x known -> (* 関数適用の場合 *)
    AppDir(Id.L(x), ys)
  | KNormal.App(f, xs) -> AppCls(f, xs)
  | KNormal.Tuple(xs) -> Tuple(xs)
  | KNormal.LetTuple(xts, y, e) -> LetTuple(xts, y, g (M.add_list xts env) known e)
  | KNormal.Get(x, y) -> Get(x, y)
  | KNormal.Put(x, y, z) -> Put(x, y, z)
  | KNormal.ExtArray(x) -> ExtArray(Id.L(x))
  | KNormal.ExtFunApp(x, ys) -> AppDir(Id.L("min_caml_" ^ x), ys)
  | KNormal.Nil -> Nil
  | KNormal.Cons(x, y) -> Cons(x, y)
  | KNormal.LetList((matcher, typ), y, e) -> LetList((matcher, typ), y, g (M.add_list_matcher matcher (ref (Some typ)) env) known e)

let f e =
  toplevel := [];
  let e' = g M.empty S.empty e in
  Prog(List.rev !toplevel, e')
