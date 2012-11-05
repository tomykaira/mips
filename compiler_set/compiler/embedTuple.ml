open Closure
(* 配列の中にタプルを埋め込むモジュール *)

let rec a = function
  | [] -> []
  | (x, Type.Array(Type.Tuple(ts)))::xs -> (x, ts)::a xs
  | _::xs -> a xs

(* 配列の中のタプルを展開する関数 *)
let rec g env env' = function
  | IfEq(x, y, e1, e2) -> IfEq(x, y, g env env' e1, g env env' e2)
  | IfLE(x, y, e1, e2) -> IfLE(x, y, g env env' e1, g env env' e2)
  | IfLT(x, y, e1, e2) -> IfLT(x, y, g env env' e1, g env env' e2)
  | Let((x, t), e1, e2) ->
      (match t with
      | Type.Array(Type.Tuple(ts)) -> Let((x,t), g env env' e1, g (M.add x ts env) (M.add x t env') e2)
      | _ -> Let((x,t), g env env' e1, g env (M.add x t env') e2))
  | MakeCls((x, t), cl, e) -> MakeCls((x, t), cl, g env (M.add x t env') e)
  | LetTuple(xts, y, e) ->
      LetTuple(xts, y, g (M.add_list (a xts) env) (M.add_list xts env') e)
  | AppDir((Id.L("min_caml_create_tuple_array") as l), [x;y]) ->
      (match M.find y env' with
      | Type.Tuple(ts) -> let xts = List.map (fun t -> (Id.genid y, t)) ts in
	                  LetTuple(xts, y, AppDir(l, x::(List.map fst xts)))
      | _ -> assert false)
  | Get(x,y) when M.mem x env ->
      let ts = M.find x env in
      let yts = List.map (fun t -> (Id.genid x,t)) ts in
      let len = Id.genid "Len" in
      let ind = Id.genid "Ind" in
      let tup = Id.genid "Tup" in
      Let((len, Type.Int), Int(List.length ts),
	  Let((ind, Type.Int), Mul(len, y),
	      Let((tup, Type.Tuple(ts)), Add(x, ind),
		  LetTuple(yts, tup,
			   Tuple(List.map fst yts)))))
  | Put(x,y,z) when M.mem x env ->
      let ts = M.find x env in
      let yts = List.map (fun t -> (Id.genid x,t)) ts in
      LetTuple(yts, z,
	       PutTuple(x, y, List.map fst yts))
  | e -> e


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
  | _ -> false

(* 本体 *)
let f (Prog(toplevel, e)) =
  (* 外部関数や配列に1つでもArray(Tuple(_))の形を持つものがあったら諦める *)
  if M.exists (fun _ -> at) !Typing.extenv then Prog(toplevel,e) else
  (Format.eprintf "embedding tuples into array...@.";
  Prog(List.map h toplevel, g M.empty M.empty e))

  
  
