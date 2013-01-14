open ANormal

(* 共通部分式除去を行うモジュール *)

(*let qlen = 10 

let dummy = (Var(Id.genid ""), "") *)

type environ = {
    precious : (exp * Id.t) list;
    cheap : (exp * Id.t) list;
    noeffect : S.t;
    tuple : (Id.t list) M.t;
  }

(* envを操作する関数群 *)
let add_precious exp x env =
  { env with precious = (exp,x)::env.precious }
(*let rec take n = function
  | [] -> []
  | x::xs -> if n <= 0 then [] else x::take (n-1) xs *)
let add_cheap exp x env =
  { env with cheap = (exp,x)::env.cheap }
(*let rec pushn n env = env
  let rec f m l = if m <= 0 || List.mem dummy l then l else f (m-1) (dummy::l) in
  { env with cheap = take' qlen (f (min n qlen) (env.cheap)) } *)
let add_tuple x ys env =
  { env with tuple = M.add x ys env.tuple }
let make_empty e env =
  let fvs = fv e in
  { env with precious = List.filter (fun (_,x) -> S.mem x fvs) env.precious;
             cheap = List.filter (fun (_,x) -> S.mem x fvs) env.cheap }


(* 式の中に関数呼び出しがあるか判定 *)
let rec ecall = function
  | Let(_,exp,e) -> ecall' exp || ecall e
  | LetRec(_,e) | LetTuple(_,_,e) | LetList(_,_,e) -> ecall e
  | Ans(exp) -> ecall' exp
and ecall' = function
  | IfEq(_, _, e1, e2) | IfLE(_, _, e1, e2) | IfLT(_, _, e1, e2) | IfNil(_, e1, e2) 
    -> ecall e1 || ecall e2
  | App _ | ExtFunApp _ -> true
  | _ -> false


let rec effect env = function (* 副作用(Get含む)の有無 *)
  | Let(_, exp, e) -> effect' env exp || effect env e
  | LetRec({ name = (x,_); args = _; body = e1 }, e) ->
      let env' = 
	let env' = S.add x env in
	if effect env' e1 then env else env' in
      effect env' e
  | LetTuple(_, _, e) | LetList(_,_,e) -> effect env e
  | Ans(exp) -> effect' env exp
and effect' env = function
  | IfEq(_, _, e1, e2) | IfLE(_, _, e1, e2) | IfLT(_, _, e1, e2) | IfNil(_,e1,e2)-> effect env e1 || effect env e2
  (* 副作用の無いライブラリ関数 *)
  | ExtFunApp (("create_array" | "create_float_array" | "create_tuple_array" | "floor" | "ceil" | "float_of_int" | "int_of_float" | "truncate" | "not" | "xor" | "sqrt"  | "atan" | "tan" | "sin" | "cos" | "log" | "exp"  | "land" | "sinh" | "cosh" | "mul" | "div_binary_search" | "div" ), _) -> false
  | App (x, _) when S.mem x env -> false
  | App _ | Put _ | Get _ | ExtFunApp _ -> true
  | _ -> false


(* 共通部分式除去を行う関数 *)
let rec g env = function
  | Let((x,_) as xt, exp, e) ->
      let exp' = g' env exp in
      (match exp' with
      | Unit | Var _ | ExtArray _ | Nil -> 
	  Let(xt, exp', g env e)
      | App(y, _) when S.mem y env.noeffect  ->
	  Let(xt, exp', g (add_precious exp' x (make_empty e env)) e)
      | ExtFunApp(("print_char"|"input_char"|"read_char"),_) ->
	  Let(xt, exp', g env e)
      | ExtFunApp(("xor"|"sqrt"|"not"),_) ->
	  Let(xt, exp', g (add_precious exp' x env) e)
      | ExtFunApp (("floor" | "ceil" | "float_of_int" | "int_of_float" | "truncate" | "atan" | "tan" | "sin" | "cos" | "log" | "exp"  | "land" | "sinh" | "cosh" | "mul" | "div_binary_search" | "div" ), _) ->
	  Let(xt, exp', g (add_precious exp' x (make_empty e env)) e)
      | App _ | ExtFunApp _ -> Let(xt, exp', g (make_empty e env) e)
      | _ ->
	  let env' =
	    let env' = if ecall' exp' then make_empty e env else env in
	    if effect' env'.noeffect exp' then env'
	    else add_precious exp' x env' in
	  Let(xt, exp', g env' e))
  | LetRec({ name = (x,_) as xt; args = yts; body = e1 }, e2) ->
      let env' =
	let eff = S.add x env.noeffect in
	if effect eff e1 then env else { env with noeffect = eff } in
      LetRec({ name = xt; args = yts; body = g (make_empty e1 env') e1 }, g env' e2)
  | LetTuple(l, y, e) ->
      if M.mem y env.tuple then
	let l' = M.find y env.tuple in
	List.fold_left2
	  (fun e xt x' -> Let(xt,Var(x'),e))
	  (g env e)
	  l
	  l'
      else LetTuple(l, y, g (add_tuple y (List.map fst l) env) e)
  | LetList(xt, y, e) ->
      LetList(xt, y, g env e)
  | Ans(exp) -> Ans(g' env exp)
and g' env exp =
  let exp' = match exp with
  | IfEq(x, y, e1, e2) -> IfEq(x, y, g env e1, g env e2)
  | IfLE(x, y, e1, e2) -> IfLE(x, y, g env e1, g env e2)
  | IfLT(x, y, e1, e2) -> IfLT(x, y, g env e1, g env e2)
  | IfNil(x, e1, e2) -> IfNil(x, g env e1, g env e2)
  | _ -> exp in
  try Var(List.assoc exp' (env.precious@env.cheap))
  with Not_found -> exp'
	

let f e = Format.eprintf "eliminating common subexpressions...@.";
          g { precious = []; cheap = []; noeffect = S.empty; tuple = M.empty } e



