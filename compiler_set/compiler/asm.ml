(*pp deriving *)
(* mimic assembly with a few virtual instructions *)

type t = (* 命令の列 *)
  | Ans of exp
  | Let of (Id.t * Type.t) * exp * t
and exp = (* 一つ一つの命令に対応する式 *)
  | Nop (* virtual instruction *)

  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | Xor of Id.t * Id.t

  | AddI of Id.t * int
  | SubI of Id.t * int
  | XorI of Id.t * int

  | Int of int      (* virtual instruction *)
  | Float of float  (* virtual instruction *)
  | SetL of Id.l    (* virtual instruction, ラベルlのアドレスを返す *)
  | SllI of Id.t * int
  | SraI of Id.t * int
  | IMovF of Id.t
  | FMovI of Id.t

  | FMov  of Id.t
  | FNeg  of Id.t
  | FAdd  of Id.t * Id.t
  | FSub  of Id.t * Id.t
  | FMul  of Id.t * Id.t 
  | FDiv  of Id.t * Id.t (* virtual instruction *)
  | FInv  of Id.t
  | FSqrt of Id.t

  | LdI of Id.t * int
  | StI of Id.t * Id.t * int
  | LdR of Id.t * Id.t
  | FLdI of Id.t * int
  | FStI of Id.t * Id.t * int
  | FLdR of Id.t * Id.t
	
  | Comment of string
	
  (* virtual instructions *)
  | IfEq  of Id.t * Id.t * t * t
  | IfLT  of Id.t * Id.t * t * t
  | IfLE  of Id.t * Id.t * t * t 
  | IfFEq of Id.t * Id.t * t * t
  | IfFLT of Id.t * Id.t * t * t
  | IfFLE of Id.t * Id.t * t * t
  (* closure address, integer arguments, and float arguments *)
  | CallCls of Id.l * Id.t * Id.t list * Id.t list
  | CallDir of Id.l * Id.t list * Id.t list
  | Save of Id.t * Id.t (* レジスタ変数の値をスタック変数へ保存 *)
  | Restore of Id.t (* スタック変数から値を復元 *)
  | SAlloc of int
      deriving (Show)

type fundef = { name : Id.l; args : Id.t list; fargs : Id.t list; body : t; ret : Type.t }
      deriving (Show)

(* プログラム全体 = トップレベル関数 + メインの式 *)
type prog = Prog of fundef list * t
      deriving (Show)

let seq(e1, e2) = Let((Id.gentmp Type.Unit, Type.Unit), e1, e2)

let regs = Array.init 27 (fun i -> Printf.sprintf "$r%d" (i+3)) 
let fregs = Array.init 31 (fun i -> Printf.sprintf "$f%d" (i+1))

let allregs = Array.to_list regs
let allfregs = Array.to_list fregs

let reg_0  = "$r0" (* fixed to 0 *)
let reg_fp = "$r1" (* frame pointer *)
let reg_hp = "$r2" (* heap pointer *)
let reg_cl = regs.(Array.length regs - 1) (* closure pointer *)
let reg_sw = regs.(Array.length regs - 2)
let reg_1  = "$r30" (* fixed to 1 *)
let reg_m1 = "$r31" (* fixed to -1 *)

let reg_f0 = "$f0" (* fixed to 0.0 *)
let reg_fsw = fregs.(Array.length fregs - 1)

let is_reg x = (x.[0] = '$')


(* nub *)
let rec remove_and_uniq xs = function
  | [] -> []
  | x :: ys when S.mem x xs -> remove_and_uniq xs ys
  | x :: ys -> x :: remove_and_uniq (S.add x xs) ys

(* free variables in the order of use (for spilling) *)
let rec fv_exp = function
  | Nop | Int(_) | Float(_) | SetL(_) | Comment(_) | SAlloc(_)-> []

  | AddI(x,_) | SubI(x,_) 
  | XorI(x,_) | SllI(x,_) | SraI(x,_) | FMov(x) | FNeg(x) | FInv(x) | FSqrt(x)
  | IMovF(x) | FMovI(x) | LdI(x,_) | FLdI(x,_) | Restore(x) -> [x]

  | Add(x,y) | Sub(x,y) | Xor(x,y)
  | FAdd(x,y) | FSub(x,y) | FMul(x,y) | FDiv(x,y) 
  | LdR(x,y) | StI(x,y,_) | FLdR(x,y) | FStI(x,y,_) | Save(x,y) -> [x;y]
 		
  | IfEq(x,y,e1,e2) | IfLT(x,y,e1,e2) | IfLE(x,y,e1,e2) | IfFEq(x,y,e1,e2)
  | IfFLT(x,y,e1,e2) | IfFLE(x,y,e1,e2)
    -> x :: y :: remove_and_uniq S.empty (fv e1 @ fv e2) (* uniq here just for efficiency *)

  | CallCls(_, x, ys, zs) -> x :: ys @ zs
  | CallDir(_, ys, zs) -> ys @ zs
and fv = function
  | Ans(exp) -> fv_exp exp
  | Let((x, t), exp, e) ->
      fv_exp exp @ remove_and_uniq (S.singleton x) (fv e)


let is_var x = not (is_reg x || x.[0] = '%')
let rec fv_var_exp = function
  | Nop | Int(_) | Float(_) | SetL(_) | Comment(_) | SAlloc(_)-> []

  | AddI(x,_) | SubI(x,_) 
  | XorI(x,_) | SllI(x,_) | SraI(x,_) | FMov(x) | FNeg(x) | FInv(x) | FSqrt(x)
  | IMovF(x) | FMovI(x) | LdI(x,_) | FLdI(x,_) | Restore(x) ->
      if is_var x then [] else [x]

  | Add(x,y) | Sub(x,y) | Xor(x,y)
  | FAdd(x,y) | FSub(x,y) | FMul(x,y) | FDiv(x,y)
  | LdR(x,y) | StI(x,y,_) | FLdR(x,y) | FStI(x,y,_) | Save(x,y) ->
      List.filter is_var [x;y]
 		
  | IfEq(x,y,e1,e2) | IfLT(x,y,e1,e2) | IfLE(x,y,e1,e2) | IfFEq(x,y,e1,e2)
  | IfFLT(x,y,e1,e2) | IfFLE(x,y,e1,e2)
    -> (List.filter is_var [x;y]) @ remove_and_uniq S.empty (fv_var e1 @ fv_var e2)
  | CallCls(_, x, ys, zs) -> List.filter is_var (x :: ys @ zs)
  | CallDir(_, ys, zs) -> List.filter is_var (ys @ zs)
and fv_var = function
  | Ans(exp) -> fv_var_exp exp
  | Let((x, t), exp, e) ->
      fv_var_exp exp @ remove_and_uniq (S.singleton x) (fv_var e)

(* そのプログラムの使うスタックの大きさを求める *)
let rec st = function
  | Ans(exp) -> st' exp
  | Let(_,exp,e) -> st' exp + st e
and st' = function
  | SAlloc(i) -> i
  | IfEq(_,_,e1,e2) | IfLT(_,_,e1,e2) | IfLE(_,_,e1,e2) | IfFEq(_,_,e1,e2)
  | IfFLT(_,_,e1,e2) | IfFLE(_,_,e1,e2) -> st e1 + st e2
  | _ -> 0
let stack_max e =
  List.length (fv_var e) + st e



let fv e = remove_and_uniq S.empty (fv e)

let rec fv_int_exp = function
  | Nop | Int(_) | Float(_) | SetL(_) | Comment(_) | FMov(_) | FNeg(_)
  | FInv(_) | FSqrt(_) | FMovI(_) | Restore(_) | FAdd(_,_) | FSub(_,_)
  | FMul(_,_) | FDiv(_,_) | Save(_,_) | SAlloc(_)
    -> []
  | AddI(x,_) | SubI(x,_) 
  | XorI(x,_) | SllI(x,_) | SraI(x,_)  | IMovF(x)  | LdI(x,_) | FLdI(x,_)
  | FStI(_,x,_)   -> [x]
  | Add(x,y) | Sub(x,y) | Xor(x,y)
  | LdR(x,y) | StI(x,y,_) | FLdR(x,y)  -> [x;y]       	
  | IfEq(x,y,e1,e2) | IfLT(x,y,e1,e2) | IfLE(x,y,e1,e2) 
    -> x :: y :: remove_and_uniq S.empty (fv_int e1 @ fv_int e2)
  | IfFEq(_,_,e1,e2) | IfFLT(_,_,e1,e2) | IfFLE(_,_,e1,e2)
    -> remove_and_uniq S.empty (fv_int e1 @ fv_int e2)
  | CallCls(_, x, ys, _) -> x :: ys 
  | CallDir(_, ys, _) -> ys 
and fv_int = function
  | Ans(exp) -> fv_int_exp exp
  | Let((x, t), exp, e) ->
      fv_int_exp exp @ remove_and_uniq (S.singleton x) (fv_int e)
let fv_int e = List.filter (fun x -> not (is_reg x) || List.mem x allregs) (remove_and_uniq S.empty (fv_int e))

let rec fv_float_exp = function
  | Nop | Int(_) | Float(_) | SetL(_) | Comment(_)  | Restore(_)
  | Add(_,_) | Sub(_,_) | Xor(_,_) | AddI(_,_) | SubI(_,_) | XorI(_,_)
  | SllI(_,_) | SraI(_,_)  | IMovF(_)  | LdI(_,_) | FLdI(_,_) | LdR(_,_)
  | StI(_,_,_) | FLdR(_,_)| Save(_,_) | SAlloc(_) -> []  
  | FMov(x) | FNeg(x) | FInv(x) | FSqrt(x) | FMovI(x) | FStI(x,_,_) -> [x]
  | FAdd(x,y) | FSub(x,y) | FMul(x,y) | FDiv(x,y)  ->
      [x;y]
  | IfEq(_,_,e1,e2) | IfLT(_,_,e1,e2) | IfLE(_,_,e1,e2) 
    -> remove_and_uniq S.empty (fv_float e1 @ fv_float e2)
  | IfFEq(x,y,e1,e2) | IfFLT(x,y,e1,e2) | IfFLE(x,y,e1,e2)
    -> x :: y :: remove_and_uniq S.empty (fv_float e1 @ fv_float e2)
  | CallCls(_, _, _, zs) | CallDir(_, _, zs) -> zs 
and fv_float = function
  | Ans(exp) -> fv_float_exp exp
  | Let((x, t), exp, e) ->
      fv_float_exp exp @ remove_and_uniq (S.singleton x) (fv_float e)
let fv_float e = List.filter (fun x -> not (is_reg x) || List.mem x allfregs) (remove_and_uniq S.empty (fv_float e))



let rec concat e1 xt e2 =
  match e1 with
  | Ans(exp) -> Let(xt, exp, e2)
  | Let(yt, exp, e1') -> Let(yt, exp, concat e1' xt e2)


