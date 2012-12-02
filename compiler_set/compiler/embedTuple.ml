open Closure
(* 配列の中にタプルを埋め込むモジュール *)

let rec a = function
  | [] -> []
  | (x, Type.Array(Type.Tuple(ts)))::xs -> (x, ts)::a xs
  | _::xs -> a xs



(* 配列の中のタプルを展開する関数 *)
let rec g env env' = function
  | Let((x, t), exp, e) ->
      (match t with
      | Type.Array(Type.Tuple(ts)) -> concat (g' env env' exp) (x,t) (g (M.add x ts env) (M.add x t env') e)
      | _ -> concat (g' env env' exp) (x,t) (g env (M.add x t env') e))
  | MakeCls((x, t), cl, e) -> MakeCls((x, t), cl, g env (M.add x t env') e)
  | LetTuple(xts, y, e) ->
      LetTuple(xts, y, g (M.add_list (a xts) env) (M.add_list xts env') e)
  | LetList((x,t), y, e) ->
      let xts = List.map (fun x -> (x,t)) (Syntax.matcher_variables x) in
      LetList((x,t), y, g (M.add_list (a xts) env) (M.add_list xts env') e)
  | Ans(exp) -> g' env env' exp
and g' env env' = function
  | IfEq(x, y, e1, e2) -> Ans(IfEq(x, y, g env env' e1, g env env' e2))
  | IfLE(x, y, e1, e2) -> Ans(IfLE(x, y, g env env' e1, g env env' e2))
  | IfLT(x, y, e1, e2) -> Ans(IfLT(x, y, g env env' e1, g env env' e2))
  | IfNil(x, e1, e2) -> Ans(IfNil(x, g env env' e1, g env env' e2))
  | AppDir((Id.L("min_caml_create_tuple_array") as l), [x;y]) ->
      (match M.find y env' with
      | Type.Tuple(ts) ->
	  let xts = List.map (fun t -> (Id.genid y, t)) ts in
	  LetTuple(xts, y, Ans(AppDir(l, x::(List.map fst xts))))
      | _ -> assert false)
  | AppDir((Id.L("min_caml_tuple_array_init") as l), [v;x;y]) ->
      (match M.find y env' with
      | Type.Tuple(ts) ->
	  let xts = List.map (fun t -> (Id.genid y, t)) ts in
	  LetTuple(xts, y, Ans(AppDir(l, v::x::(List.map fst xts))))
      | _ -> assert false)
  | Get(x,y) when M.mem x env -> 
      let z = Id.genid x in
      let ts = M.find x env in
      let xts = List.map (fun t -> (Id.genid x, t)) ts in
      Let((z,Type.Tuple(ts)), GetTuple(x,y),
	  LetTuple(xts, z,
		   Ans(Tuple(List.map fst xts))))
  | Put(x,y,z) when M.mem x env ->
      let ts = M.find x env in
      let yts = List.map (fun t -> (Id.genid x,t)) ts in
      LetTuple(yts, z,
	       Ans(PutTuple(x, y, List.map fst yts)))
  | exp -> Ans(exp)


(* 関数の中のタプルの配列を展開する関数 *)
let h { name = xt; args = yts; formal_fv = zts; body = e } =
  { name = xt; args = yts; formal_fv = zts;
    body = g (M.add_list (a (yts@zts)) M.empty) (M.add_list (yts@zts) M.empty) e }


(* 型の中にArray(Tuple(_))の形が現れているか判定する関数 *)
let rec at = function
  | Type.Array(Type.Tuple(_)) -> true
  | Type.Fun(ts,t) -> List.exists at (t::ts)
  | Type.Tuple(ts) -> List.exists at ts
  | Type.Array(t) -> at t
  | Type.List(t) ->
      (match !t with
      | Some t' -> at t'
      | _ -> false)
  | _ -> false


(* その数が2の何乗か返す(切り上げ) *)
let rec log2_sub n i =
  if 1 lsl i >= n then i
  else log2_sub n (i+1)
let log2 n = log2_sub n 0

(* グローバル配列のタプルを展開 *)
let rec i { gname = (x, t); length = l } =
  let l' = match t with
  | Type.Array(Type.Tuple(ts)) -> (1 lsl (log2 (List.length ts))) * l
  | _ -> l in
  { gname = (x, t); length = l' }


(* 本体 *)
let f (Prog(globals, toplevel, e)) =
  (* 外部関数や配列に1つでもArray(Tuple(_))の形を持つものがあったら諦める *)
  if M.exists (fun _ -> at) !Typing.extenv then Prog(globals,toplevel,e) else
  (Format.eprintf "embedding tuples into array...@.";
  Prog(List.map i globals, List.map h toplevel, g M.empty M.empty e))

  
  
