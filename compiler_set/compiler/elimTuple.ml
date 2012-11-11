open Closure

(* ベータ変換をする関数 *)
let find x env = try M.find x env with Not_found -> x

(* 不要なタプルを除去する関数 *)
let rec g env1 env2= function
  | Neg(x) -> Neg(find x env2)
  | Add(x, y) -> Add(find x env2, find y env2)
  | Sub(x, y) -> Sub(find x env2, find y env2)
  | Mul(x, y) -> Mul(find x env2, find y env2)
  | Sll(x, i) -> Sll(find x env2, i)
  | Sra(x, i) -> Sra(find x env2, i)

  | FNeg(x) -> FNeg(find x env2)
  | FAdd(x, y) -> FAdd(find x env2, find y env2)
  | FSub(x, y) -> FSub(find x env2, find y env2)
  | FMul(x, y) -> FMul(find x env2, find y env2)
  | FDiv(x, y) -> FDiv(find x env2, find y env2)

  | IfEq(x, y, e1, e2) ->
      IfEq(find x env2, find y env2, g env1 env2 e1, g env1 env2 e2)
  | IfLE(x, y, e1, e2) ->
      IfLE(find x env2, find y env2, g env1 env2 e1, g env1 env2 e2)
  | IfLT(x, y, e1, e2) ->
      IfLT(find x env2, find y env2, g env1 env2 e1, g env1 env2 e2)
  | Let((x,t), Tuple(ys), e2) ->
      let ys' = List.map (fun y -> find y env2) ys in
      let env1' = M.add x ys' env1 in
      let e2' = g env1' env2 e2 in
      if S.mem x (fv e2') then Let((x,t), Tuple(ys'), e2')
      else e2'
  | Let(xt, e1, e2) -> Let(xt, g env1 env2 e1, g env1 env2 e2)

  | Var(x) -> Var(find x env2)

  | MakeCls((x, t), { entry = l; actual_fv = ys }, e) ->
       MakeCls((x, t),
	       { entry = l; actual_fv = List.map (fun y -> find y env2) ys },
	       g env1 env2 e)
  | AppCls(x, ys) -> AppCls(find x env2, List.map (fun y -> find y env2) ys)
  | AppDir(l, ys) -> AppDir(l, List.map (fun y -> find y env2) ys)

  | Tuple(xs) -> Tuple(List.map (fun x -> find x env2) xs)
  | LetTuple(xts, y, e) ->
      let y' = find y env2 in
      if M.mem y' env1 then
	g env1 (M.add_list (List.combine (List.map fst xts) (M.find y' env1)) env2) e
      else LetTuple(xts, y', g env1 env2 e)
  | Get(x, y) -> Get(find x env2, find y env2)
  | Put(x, y, z) -> Put(find x env2, find y env2, find z env2)
  | GetTuple(x, y) ->
      GetTuple(find x env2, find y env2)
  | PutTuple(x, y, zs) ->
      PutTuple(find x env2, find y env2, List.map (fun z -> find z env2) zs)
  | e -> e


(* 本体 *)
let f (Prog(toplevel, e)) =
  Format.eprintf "eliminating tuples...@.";
  Prog(List.map (fun { name = xt; args = yts; formal_fv = zts; body = e } -> { name = xt; args = yts; formal_fv = zts; body = g M.empty M.empty e }) toplevel,
    g M.empty M.empty e)
