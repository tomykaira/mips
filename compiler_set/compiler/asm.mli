(*pp deriving *)

type t =
  | Ans of exp
  | Let of (Id.t * Type.t) * exp * t
and exp = (* 一つ一つの命令に対応する式 *)
  | Nop
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

  | Int of int
  | Float of float
  | SetL of Id.l 
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
  | FDiv of Id.t * Id.t
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
  | Save of Id.t * Id.t 
  | Restore of Id.t
      deriving (Show)

type fundef = { name : Id.l; args : Id.t list; fargs : Id.t list; body : t; ret : Type.t }
    deriving (Show)
type prog = Prog of fundef list * t
    deriving (Show)

val fletd : Id.t * exp * t -> t (* shorthand of Let for float *)
val seq : exp * t -> t (* shorthand of Let for unit *)

val regs : Id.t array
val fregs : Id.t array
val allregs : Id.t list
val allfregs : Id.t list
val reg_0 : Id.t
val reg_fp : Id.t
val reg_hp : Id.t
val reg_cl : Id.t
val reg_sw : Id.t
val reg_fsw : Id.t
val reg_1 : Id.t
val reg_m1 :Id.t
val is_reg : Id.t -> bool

val fv : t -> Id.t list
val concat : t -> Id.t * Type.t -> t -> t
