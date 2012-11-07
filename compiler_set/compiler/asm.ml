(*pp deriving *)

(* mimic assembly with a few virtual instructions *)

type t = (* 命令の列 *)
  | Ans of exp
  | Let of (Id.t * Type.t) * exp * t
and exp = (* 一つ一つの命令に対応する式 *)
  | Nop (* virtual instruction *)

  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | Mul of Id.t * Id.t
  | And of Id.t * Id.t
  | Or  of Id.t * Id.t
  | Nor of Id.t * Id.t
  | Xor of Id.t * Id.t

  | AddI of Id.t * int
  | SubI of Id.t * int
  | MulI of Id.t * int
  | AndI of Id.t * int
  | OrI  of Id.t * int
  | NorI of Id.t * int
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
  | FMulN of Id.t * Id.t 
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
  | CallCls of Id.t * Id.t list * Id.t list
  | CallDir of Id.l * Id.t list * Id.t list
  | Save of Id.t * Id.t (* レジスタ変数の値をスタック変数へ保存 *)
  | Restore of Id.t (* スタック変数から値を復元 *)
      deriving (Show)

type fundef = { name : Id.l; args : Id.t list; fargs : Id.t list; body : t; ret : Type.t }
      deriving (Show)

(* プログラム全体 = トップレベル関数 + メインの式 *)
type prog = Prog of fundef list * t
      deriving (Show)

let fletd(x, e1, e2) = Let((x, Type.Float), e1, e2)
let seq(e1, e2) = Let((Id.gentmp Type.Unit, Type.Unit), e1, e2)

let regs = Array.init 27 (fun i -> Printf.sprintf "$r%d" (i+3)) 
let fregs = Array.init 32 (fun i -> Printf.sprintf "$f%d" i)

let allregs = Array.to_list regs
let allfregs = Array.to_list fregs

let reg_0  = "$r0" (* fixed to 0 *)
let reg_fp = "$r1" (* frame pointer *)
let reg_hp = "$r2" (* heap pointer *)
let reg_cl = regs.(Array.length regs - 1) (* closure pointer *)
let reg_sw = regs.(Array.length regs - 2)
let reg_fsw = fregs.(Array.length regs - 1)
let reg_1  = "$r30" (* fixed to 1 *)
let reg_m1 = "$r31" (* fixed to -1 *)

let is_reg x = (x.[0] = '$')


(* nub *)
let rec remove_and_uniq xs = function
  | [] -> []
  | x :: ys when S.mem x xs -> remove_and_uniq xs ys
  | x :: ys -> x :: remove_and_uniq (S.add x xs) ys

(* free variables in the order of use (for spilling) *)
let rec fv_exp = function
  | Nop | Int(_) | Float(_) | SetL(_) | Comment(_) -> []

  | AddI(x,_) | SubI(x,_) | MulI(x,_) | AndI(x,_) | OrI(x,_) | NorI(x,_)
  | XorI(x,_) | SllI(x,_) | SraI(x,_) | FMov(x) | FNeg(x) | FInv(x) | FSqrt(x)
  | IMovF(x) | FMovI(x) | LdI(x,_) | FLdI(x,_) | Restore(x) -> [x]

  | Add(x,y) | Sub(x,y) | Mul(x,y) | And(x,y) | Or(x,y) | Nor(x,y) | Xor(x,y)
  | FAdd(x,y) | FSub(x,y) | FMul(x,y) | FMulN(x,y) | FDiv(x,y)
  | LdR(x,y) | StI(x,y,_) | FLdR(x,y) | FStI(x,y,_) | Save(x,y) -> [x;y]
 		
  | IfEq(x,y,e1,e2) | IfLT(x,y,e1,e2) | IfLE(x,y,e1,e2) | IfFEq(x,y,e1,e2)
  | IfFLT(x,y,e1,e2) | IfFLE(x,y,e1,e2)
    -> x :: y :: remove_and_uniq S.empty (fv e1 @ fv e2) (* uniq here just for efficiency *)

  | CallCls(x, ys, zs) -> x :: ys @ zs
  | CallDir(_, ys, zs) -> ys @ zs

and fv = function
  | Ans(exp) -> fv_exp exp
  | Let((x, t), exp, e) ->
      fv_exp exp @ remove_and_uniq (S.singleton x) (fv e)

let fv e = remove_and_uniq S.empty (fv e)

let rec concat e1 xt e2 =
  match e1 with
  | Ans(exp) -> Let(xt, exp, e2)
  | Let(yt, exp, e1') -> Let(yt, exp, concat e1' xt e2)



(* デバッグ用関数. expを出力. nは深さ. *)
let rec ind m = if m <= 0 then ()
                else (Printf.eprintf "  "; ind (m-1))
let rec dbprint n exp =
  ind n;
  match exp with
  | Nop -> Printf.eprintf "Nop\n%!"

  | Add (a, b) -> Printf.eprintf "Add %s %s\n%!" a b
  | Sub (a, b) -> Printf.eprintf "Sub %s %s\n%!" a b
  | Mul (a, b) -> Printf.eprintf "Mul %s %s\n%!" a b
  | And (a, b) -> Printf.eprintf "And %s %s\n%!" a b
  | Or  (a, b) -> Printf.eprintf "Or %s %s\n%!" a b
  | Nor (a, b) -> Printf.eprintf "Nor %s %s\n%!" a b
  | Xor (a, b) -> Printf.eprintf "Xor %s %s\n%!" a b

  | AddI (a, b) -> Printf.eprintf "AddI %s %d\n%!" a b
  | SubI (a, b) -> Printf.eprintf "SubI %s %d\n%!" a b
  | MulI (a, b) -> Printf.eprintf "MulI %s %d\n%!" a b
  | AndI (a, b) -> Printf.eprintf "AndI %s %d\n%!" a b
  | OrI  (a, b) -> Printf.eprintf "OrI %s %d\n%!" a b
  | NorI (a, b) -> Printf.eprintf "NorI %s %d\n%!" a b
  | XorI (a, b) -> Printf.eprintf "XorI %s %d\n%!" a b

  | Int a -> Printf.eprintf "Int %d\n%!" a
  | Float a-> Printf.eprintf "Float %f\n%!" a
  | SetL (Id.L l) -> Printf.eprintf "SetL L %s\n%!" l
  | SllI (a, b) -> Printf.eprintf "Sll %s %d\n%!" a b
  | SraI (a, b) -> Printf.eprintf "Sar %s %d\n%!" a b
  | IMovF (a) -> Printf.eprintf "IMovF %s\n%!" a
  | FMovI (a) -> Printf.eprintf "FMovI %s\n%!" a

  | FMov a -> Printf.eprintf "FMov %s\n%!" a
  | FNeg a -> Printf.eprintf "FNeg %s\n%!" a
  | FAdd (a, b) -> Printf.eprintf "FAdd %s %s\n%!" a b
  | FSub (a, b) -> Printf.eprintf "FSub %s %s\n%!" a b
  | FMul (a, b) -> Printf.eprintf "FMul %s %s\n%!" a b
  | FMulN (a, b) -> Printf.eprintf "FMulN %s %s\n%!" a b
  | FDiv (a, b) -> Printf.eprintf "FDiv %s %s\n%!" a b
  | FInv a -> Printf.eprintf "FInv %s\n%!" a
  | FSqrt a -> Printf.eprintf "FSqrt %s\n%!" a

  | LdI (a, b) -> Printf.eprintf "LdI %s %d\n%!" a b
  | StI (a, b, c) -> Printf.eprintf "LdI %s %s %d\n%!" a b c
  | LdR (a, b) -> Printf.eprintf "LdR %s %s\n%!" a b
  | FLdI (a, b) -> Printf.eprintf "FLdI %s %d\n%!" a b
  | FStI (a, b, c) -> Printf.eprintf "FStI %s %s %d\n%!" a b c
  | FLdR (a, b) -> Printf.eprintf "FLdR %s %s\n%!" a b

  | Comment s -> Printf.eprintf "Comment %s\n%!" s

  | IfEq (a, b, p, q) -> Printf.eprintf "If %s = %s Then\n%!" a b;
                         dbprint2 (n+1) p; ind n; Printf.eprintf "Else\n%!"; dbprint2 (n+1) q
  | IfLE (a, b, p, q) -> Printf.eprintf "If %s <= %s Then\n%!" a b;
                         dbprint2 (n+1) p; ind n; Printf.eprintf "Else\n%!"; dbprint2 (n+1) q
  | IfLT (a, b, p, q) -> Printf.eprintf "If %s < %s Then\n%!" a b;
                         dbprint2 (n+1) p; ind n; Printf.eprintf "Else\n%!"; dbprint2 (n+1) q
  | IfFEq (a, b, p, q) -> Printf.eprintf "IfF %s = %s Then\n%!" a b;
                         dbprint2 (n+1) p; ind n; Printf.eprintf "Else\n%!"; dbprint2 (n+1) q
  | IfFLE (a, b, p, q) -> Printf.eprintf "IfF %s <= %s Then\n%!" a b;
                         dbprint2 (n+1) p; ind n; Printf.eprintf "Else\n%!"; dbprint2 (n+1) q
  | IfFLT (a, b, p, q) -> Printf.eprintf "IfF %s < %s Then\n%!" a b;
                         dbprint2 (n+1) p; ind n; Printf.eprintf "Else\n%!"; dbprint2 (n+1) q


  | CallCls (a, i, f) -> Printf.eprintf "CallCls %s  I(%s), F(%s)\n%!" a (String.concat "," i) (String.concat "," f)
  | CallDir (Id.L l, i, f) -> Printf.eprintf "CallDir %s  I(%s), F(%s)\n%!" l (String.concat "," i) (String.concat "," f)
  | Save (a, b) -> Printf.eprintf "Save %s %s\n%!" a b
  | Restore (a) -> Printf.eprintf "Restore %s\n%!" a


(* デバッグ用関数. tを出力 *)
and dbprint2 n t =
  ind n;
  match t with
  | Ans exp -> Printf.eprintf "Ans\n%!"; dbprint (n+1) exp
  | Let ((x, y), exp, s) -> Printf.eprintf "Let %s:%s =\n%!" x (Type.show y);
                            dbprint (n+1) exp; dbprint2 n s

(* デバッグ用関数. fundefを出力. *)
let dbprint3 f =
  ind 1;
  match f.name with
    Id.L a -> Printf.eprintf "L %s (IArgs = (%s), FArgs = (%s) ret : {%s}) =\n%!" a (String.concat "," f.args) (String.concat "," f.fargs) (Type.show f.ret);
           dbprint2 2 f.body
