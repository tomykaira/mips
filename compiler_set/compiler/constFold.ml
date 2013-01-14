open ANormal

(* 定数畳み込みと、x + 0 = x 等の簡約を行うモジュール *)


let memi x env =
  try (match M.find x env with Int(_) -> true | _ -> false)
  with Not_found -> false
let memf x env =
  try (match M.find x env with Float(_) -> true | _ -> false)
  with Not_found -> false
let memt x env =
  try (match M.find x env with Tuple(_) -> true | _ -> false)
  with Not_found -> false
let meml x env =
  try (match M.find x env with Nil | Cons(_, _) -> true | _ -> false)
  with Not_found -> false
let memn x env =
  try (match M.find x env with Neg(_) | FNeg(_) -> true | _ -> false)
  with Not_found -> false

let findi x env = (match M.find x env with Int(i) -> i | _ -> raise Not_found)
let findf x env = (match M.find x env with Float(d) -> d | _ -> raise Not_found)
let findt x env = (match M.find x env with Tuple(ys) -> ys | _ -> raise Not_found)
let findl x env =
  let found = M.find x env in
  match found with
    | Nil | Cons(_, _) -> found
    | _ -> raise Not_found
let findn x env = (match M.find x env with Neg(x) | FNeg(x) -> x | _ -> raise Not_found)


(* 葉が全て直結した即値か調べる関数 *)
let rec allimm  = function
  | Let(_,_,e) | LetRec(_,e) | LetTuple(_,_,e) | LetList(_,_,e) -> false
  | Ans(exp) -> allimm' exp
and allimm' = function
  | Int _ | Float _ -> true
  | IfEq(_,_,e1,e2) | IfLE(_,_,e1,e2) | IfLT(_,_,e1,e2) -> allimm e1 && allimm e2
  | _ -> false


(* envifに追加するデータ型を返す関数 *)
let rec imm = function
  | Let(_,_,e) | LetRec(_,e) | LetTuple(_,_,e) | LetList(_,_,e) -> imm e
  | Ans(exp) -> Ans(imm' exp)
and imm' = function
  | Int _ | Float _ as exp -> exp
  | IfEq(x,y,e1,e2) -> IfEq(x, y, imm e1, imm e2)
  | IfLE(x,y,e1,e2) -> IfLE(x, y, imm e1, imm e2)
  | IfLT(x,y,e1,e2) -> IfLT(x, y, imm e1, imm e2)
  | _ -> assert false
(* envifに式を当てはめる関数 *)
let rec replace con = function
  | Ans((Int _ | Float _) as i) -> con i
  | Ans(IfEq(p,q,e3,e4)) ->
      Ans(IfEq(p,q,replace con e3, replace con e4))
  | Ans(IfLE(p,q,e3,e4)) ->
      Ans(IfLE(p,q,replace con e3, replace con e4))
  | Ans(IfLT(p,q,e3,e4)) ->
      Ans(IfLT(p,q,replace con e3, replace con e4))
  | _ -> assert false

(* envleに関係を追加 *)
let addle (x,y) envle =
  let envle' =
    List.fold_left
      (fun envle (z,w) -> if y = z then (x,w)::envle else if x = w then (z,y)::envle else envle)
      envle
      envle in
  (x,y)::envle'

let sc f x y = if compare x y > 0 then f x y else f y x

let rec g env envle envne envif = function (* 定数畳み込み等を行うルーチン本体 *)
  | Let((x, _) as xt, exp, e) -> (* letのケース *)
      let e1' = g' env envle envne envif exp in
      let (env', envif') =
	(match e1' with
        | Ans(Int _ | Float _ | Tuple _ | Nil | Cons _ | Neg _ | FNeg _ | ExtFunApp("not",_) as exp') -> (M.add x exp' env, envif)
	| Ans(IfEq _) | Ans(IfLE _) | Ans(IfLT _) when allimm e1' ->
	    (env, M.add x (imm e1') envif)
	| _ -> (env, envif)) in
      let e' = g env' envle envne envif' e in
      concat e1' xt e'
  | LetRec({ name = x; args = ys; body = e1 }, e2) ->
      LetRec({ name = x; args = ys; body = g env envle envne envif e1 },
	     g env envle envne envif e2)
  | LetTuple(xts, y, e) when memt y env ->
      List.fold_left2
	(fun e' xt z -> Let(xt, Var(z), e'))
	(g env envle envne envif e)
	xts
	(findt y env)
  | LetTuple(xts, y, e) -> LetTuple(xts, y, g (M.add y (Tuple(List.map fst xts)) env) envle envne envif e)
  | LetList(xts, y, e) -> LetList(xts, y, g env envle envne envif e)
  | Ans(exp) -> g' env envle envne envif exp
and g' env envle envne envif = function
  | Var(x) when memi x env -> Ans(Int(findi x env))
  | Var(x) when memf x env -> Ans(Float(findf x env))
  | Neg(x) when memi x env -> Ans(Int(-(findi x env)))
  | Neg(x) when memn x env -> Ans(Var(findn x env))

  | Add(x, y) when memi x env && memi y env -> Ans(Int(findi x env + findi y env)) 
  | Add(x, y) when memi x env && findi x env = 0 -> Ans(Var(y))
  | Add(x, y) when memi y env && findi y env = 0 -> Ans(Var(x))
  | Add(x, y) when memn x env -> Ans(Sub(y,findn x env))
  | Add(x, y) when memn y env -> Ans(Sub(x,findn y env))
  | Add(x, y) -> sc (fun x y -> Ans(Add(x,y))) x y

  | Sub(x, y) when memi x env && memi y env -> Ans(Int(findi x env - findi y env))
  | Sub(x, y) when memi y env && findi y env = 0 -> Ans(Var(x))
  | Sub(x, y) when x = y -> Ans(Int(0))
  | Sub(x, y) when memn y env -> Ans(Add(x,findn y env))

  | Mul(x, y) when memi x env && memi y env -> Ans(Int(findi x env * findi y env))
  | Mul(x, y) when memi x env && findi x env = 1 -> Ans(Var(y))
  | Mul(x, y) when memi y env && findi y env = 1 -> Ans(Var(x))
  | Mul(x, _) | Mul(_, x) when memi x env && findi x env = 0 -> Ans(Int(0))
  | Mul(x, y) -> sc (fun x y -> Ans(Mul(x,y))) x y

  | Sll(x, y) when memi x env -> (* 安全そうなら畳み込み *)
      let i = findi x env in
      if (Int32.shift_left (Int32.of_int i) y) = Int32.of_int (i lsl y) then
	Ans(Int(i lsl y))
      else Ans(Sll(x, y))
  | Sll(x, y) when y = 0 -> Ans(Var(x))
  | Sra(x, y) when memi x env -> Ans(Int((findi x env) asr y)) 
  | Sra(x, y) when y = 0 -> Ans(Var(x))

  | FNeg(x) when memf x env -> Ans(Float(-.(findf x env)))
  | FNeg(x) when memn x env -> Ans(Var(findn x env))

  | FAdd(x, y) when memf x env && memf y env -> Ans(Float(findf x env +. findf y env))
  | FAdd(x, y) when memf x env && findf x env = 0.0 -> Ans(Var(y))
  | FAdd(x, y) when memf y env && findf y env = 0.0 -> Ans(Var(x))
  | FAdd(x, y) when memn x env -> Ans(FSub(y,findn x env))
  | FAdd(x, y) when memn y env -> Ans(FSub(x,findn y env))
  | FAdd(x, y) -> sc (fun x y -> Ans(FAdd(x,y))) x y

  | FSub(x, y) when memf x env && memf y env -> Ans(Float(findf x env -. findf y env))
  | FSub(x, y) when memf y env && findf y env = 0.0 -> Ans(Var(x))
  | FSub(x, y) when memf x env && findf x env = 0.0 -> Ans(FNeg(y))
  | FSub(x, y) when memn y env -> Ans(FAdd(x,findn y env))

(* シミュレータと結果が異なってしまうので中止 *)
(*  | FMul(x, y) when memf x env && memf y env -> Ans(Float(findf x env *. findf y env)) *)
  | FMul(x, y) when memf x env && findf x env = 1.0 -> Ans(Var(y))
  | FMul(x, y) when memf y env && findf y env = 1.0 -> Ans(Var(x))
  | FMul(x, y) when memf x env && findf x env = -1.0 -> Ans(FNeg(y))
  | FMul(x, y) when memf y env && findf y env = -1.0 -> Ans(FNeg(x)) 
  | FMul(x, _) | FMul(_, x) when memf x env && findf x env = 0.0 -> Ans(Float(0.0)) 
  | FMul(x, y) -> sc (fun x y -> Ans(FMul(x,y))) x y

  | FDiv(x, y) when memf x env && memf y env -> Ans(Float(findf x env /. findf y env)) 
  | FDiv(x, y) when memf y env && findf y env = 1.0 -> Ans(Var(x))
  | FDiv(x, y) when memf y env && findf y env = -1.0 -> Ans(FNeg(x)) 

  | IfEq(x, y, e1, e2) when M.mem x envif && (memi y env || memf y env) ->
      let eq x y = match (x, y) with
      | Int(i), Int(j) -> i = j
      | Float(i), Float(j) -> i = j
      | _ -> assert false in
      g env envle envne envif
	(replace (fun i -> if eq i (M.find y env) then e1 else e2) (M.find x envif))
  | IfEq(x, y, e1, e2) when M.mem y envif && (memi x env || memf x env) ->
      g' env envle envne envif (IfEq (y,x,e1,e2))
  | IfEq(x, y, e1, e2) when memi x env && memi y env -> if findi x env = findi y env then g env envle envne envif e1 else g env envle envne envif e2
  | IfEq(x, y, e1, e2) when memi x env ->
      Ans(IfEq(x, y, g (M.add y (Int(findi x env)) env) envle envne envif e1, g env envle envne envif e2))
  | IfEq(x, y, e1, e2) when memi y env ->
      Ans(IfEq(x, y, g (M.add x (Int(findi y env)) env) envle envne envif e1, g env envle envne envif e2))
  | IfEq(x, y, e1, e2) when memf x env && memf y env -> if findf x env = findf y env then g env envle envne envif e1 else g env envle envne envif e2
  | IfEq(x, y, e1, e2) when memf x env ->
      Ans(IfEq(x, y, g (M.add y (Float(findf x env)) env) envle envne envif e1, g env envle envne envif e2))
  | IfEq(x, y, e1, e2) when memf y env ->
      Ans(IfEq(x, y, g (M.add x (Float(findf y env)) env) envle envne envif e1, g env envle envne envif e2))
  | IfEq(x, y, e1, _) when x = y || (List.mem (x,y) envle && List.mem (y,x) envle) -> g env envle envne envif e1
  | IfEq(x, y, _, e2) when List.mem (x,y) envne || List.mem (y,x) envne ->
      g env envle envne envif e2
  | IfEq(x, y, e1, e2) ->
      let e1' = g env envle envne envif e1 in
      let e2' = g env envle ((x,y)::envne) envif e2 in
      if Beta.same M.empty e1' e2' then e1' else 
      Ans(IfEq(x, y, e1', e2'))

  | IfLE(x, y, e1, e2) when M.mem x envif && (memi y env || memf y env) ->
      let le x y = match (x, y) with
      | Int(i), Int(j) -> i <= j
      | Float(i), Float(j) -> i <= j
      | _ -> assert false in
      g env envle envne envif
	(replace (fun i -> if le i (M.find y env) then e1 else e2) (M.find x envif))
  | IfLE(x, y, e1, e2) when M.mem y envif && (memi x env || memf x env) ->
      g' env envle envne envif (IfLT(y, x, e2, e1))
  | IfLE(x, y, e1, e2) when memi x env && memi y env -> if findi x env <= findi y env then g env envle envne envif e1 else g env envle envne envif e2
  | IfLE(x, y, e1, e2) when memf x env && memf y env -> if findf x env <= findf y env then g env envle envne envif e1 else g env envle envne envif e2
  | IfLE(x, y, e1, _) when x = y || List.mem (x,y) envle -> g env envle envne envif e1
  | IfLE(x, y, e1, e2) when List.mem (y,x) envle -> g' env envle envne envif (IfEq(x,y,e1,e2))
  | IfLE(x, y, e1, e2) ->
      let e1' = g env (addle (x,y) envle) envne envif e1 in
      let e2' = g env (addle (y,x) envle) ((x,y)::envne) envif e2 in
      if Beta.same M.empty e1' e2' then e1' else 
      Ans(IfLE(x, y, e1', e2')) 


  | IfLT(x, y, e1, e2) when M.mem x envif && (memi y env || memf y env) ->
      let lt x y = match (x, y) with
      | Int(i), Int(j) -> i < j
      | Float(i), Float(j) -> i < j
      | _ -> assert false in
      g env envle envne envif
	(replace (fun i -> if lt i (M.find y env) then e1 else e2) (M.find x envif))
  | IfLT(x, y, e1, e2) when M.mem y envif && (memi x env || memf x env) ->
      g' env envle envne envif (IfLE(y, x, e2, e1))
  | IfLT(x, y, e1, e2) when memi x env && memi y env -> if findi x env < findi y env then g env envle envne envif e1 else g env envle envne envif e2
  | IfLT(x, y, e1, e2) when memf x env && memf y env -> if findf x env < findf y env then g env envle envne envif e1 else g env envle envne envif e2
  | IfLT(x, y, e1, _) when List.mem (x,y) envle && (List.mem (x,y) envne || List.mem (y,x) envne) -> g env envle envne envif e1
  | IfLT(x, y, _, e2) when x = y || List.mem (y,x) envle -> g env envle envne envif e2 
  | IfLT(x, y, e1, e2) -> 
      let e1' = g env (addle (x,y) envle) ((x,y)::envne) envif e1 in
      let e2' = g env (addle (y,x) envle) envne envif e2 in
      if Beta.same M.empty e1' e2' then e1' else 
      Ans(IfLE(y, x, e2', e1'))  


  | IfNil(x, e1, e2) when meml x env -> if findl x env = Nil then g env envle envne envif e1 else g env envle envne envif e2
  | IfNil(x, e1, e2) -> Ans(IfNil(x, g env envle envne envif e1, g env envle envne envif e2))

  | ExtFunApp("xor", [x;y]) when memi x env && memi y env ->
      Ans(Int((findi x env) lxor (findi y env)))
  | ExtFunApp("xor", [x;y]) when memi x env && findi x env = 0 -> Ans(Var(y))
  | ExtFunApp("xor", [x;y]) when memi y env && findi y env = 0 -> Ans(Var(x))
  | ExtFunApp("xor", [x;y]) -> sc (fun x y -> Ans(ExtFunApp("xor", [x;y]))) x y

  | ExtFunApp("not", [x]) when memi x env -> Ans(Int(1 - (findi x env)))
  | ExtFunApp("not", [x]) as exp when M.mem x env -> (match M.find x env with
    | ExtFunApp("not", [y]) -> Ans(Var(y))
    | _ -> Ans(exp))
  | ExtFunApp("lsl", [x;y]) when memi y env -> Ans(Sll(x, findi y env))
  | ExtFunApp("lsr", [x;y]) when memi y env -> Ans(Sra(x, findi y env))
  | e -> Ans(e)

let f e = Format.eprintf "Constant Folding...@.";
          g M.empty [] [] M.empty e
