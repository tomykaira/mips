(* 実際の出力を行うモジュール *)




(* アセンブリコードの型 *)
type exp = (* 一つ一つの命令に対応する式 *)
  (* アセンブラが処理するニーモニックやラベル *)
  | Label of string
  | SetL of Id.t * string
  | Nop
  | Comment of string

  | Add of Id.t * Id.t * Id.t
  | Sub of Id.t * Id.t * Id.t
  | Mul of Id.t * Id.t * Id.t
  | And of Id.t * Id.t * Id.t
  | Or  of Id.t * Id.t * Id.t
  | Nor of Id.t * Id.t * Id.t
  | Xor of Id.t * Id.t * Id.t

  | AddI of Id.t * Id.t * int
  | SubI of Id.t * Id.t * int
  | MulI of Id.t * Id.t * int
  | AndI of Id.t * Id.t * int
  | OrI  of Id.t * Id.t * int
  | NorI of Id.t * Id.t * int
  | XorI of Id.t * Id.t * int

  | Mvlo of Id.t * int
  | Mvhi of Id.t * int
  | FMvlo of Id.t * int 
  | FMvhi of Id.t * int

  | SllI of Id.t * Id.t * int
  | SraI of Id.t * Id.t * int
  | IMovF of Id.t * Id.t
  | FMovI of Id.t * Id.t

  | FMov  of Id.t * Id.t
  | FNeg  of Id.t * Id.t
  | FAdd  of Id.t * Id.t * Id.t
  | FSub  of Id.t * Id.t * Id.t
  | FMul  of Id.t * Id.t * Id.t
  | FMulN of Id.t * Id.t * Id.t
  | FInv  of Id.t * Id.t
  | FSqrt of Id.t * Id.t

  | LdI of Id.t * Id.t * int
  | StI of Id.t * Id.t * int
  | LdR of Id.t * Id.t * Id.t
  | FLdI of Id.t * Id.t * int
  | FStI of Id.t * Id.t * int
  | FLdR of Id.t * Id.t * Id.t
	
  | BEq of Id.t * Id.t * string
  | BLT of Id.t * Id.t * string
  | BLE of Id.t * Id.t * string
  | FBEq of Id.t * Id.t * string
  | FBLT of Id.t * Id.t * string
  | FBLE of Id.t * Id.t * string
	
  | J of string
  | Jr of Id.t
  | Call of string
  | CallR of Id.t
  | Return
  | Inputb of Id.t
  | Outputb of Id.t
  | Halt
type t = exp list

let print buf = function
  | AddI(x,y,i) | SubI(x,y,i) when x = y && i = 0 -> ()
  | t -> buf := t::!buf

let o oc = function
  | Label l -> Printf.fprintf oc "%s:\n" l
  | SetL (x, l)  -> Printf.fprintf oc "\tsetl\t%s, %s\n" x l
  | Nop -> Printf.fprintf oc "\tnop\n"
  | Comment s   -> Printf.fprintf oc "#%s\n" s

  | Add (x, y, z) -> Printf.fprintf oc "\tadd\t%s, %s, %s\n" x y z
  | Sub (x, y, z) -> Printf.fprintf oc "\tsub\t%s, %s, %s\n" x y z
  | Mul (x, y, z) -> Printf.fprintf oc "\tmul\t%s, %s, %s\n" x y z
  | And (x, y, z) -> Printf.fprintf oc "\tand\t%s, %s, %s\n" x y z
  | Or  (x, y, z) -> Printf.fprintf oc "\tor\t%s, %s, %s\n" x y z
  | Nor (x, y, z) -> Printf.fprintf oc "\tnor\t%s, %s, %s\n" x y z
  | Xor (x, y, z) -> Printf.fprintf oc "\txor\t%s, %s, %s\n" x y z

  | AddI (x, y, i) -> Printf.fprintf oc "\taddi\t%s, %s, %d\n" x y i
  | SubI (x, y, i) -> Printf.fprintf oc "\tsubi\t%s, %s, %d\n" x y i
  | MulI (x, y, i) -> Printf.fprintf oc "\tmuli\t%s, %s, %d\n" x y i
  | AndI (x, y, i) -> Printf.fprintf oc "\tandi\t%s, %s, %d\n" x y i
  | OrI  (x, y, i) -> Printf.fprintf oc "\tori\t%s, %s, %d\n" x y i
  | NorI (x, y, i) -> Printf.fprintf oc "\tnori\t%s, %s, %d\n" x y i
  | XorI (x, y, i) -> Printf.fprintf oc "\txori\t%s, %s, %d\n" x y i

  | Mvlo (x, i) -> Printf.fprintf oc "\tmvlo\t%s, %d\n" x i
  | Mvhi (x, i) -> Printf.fprintf oc "\tmvhi\t%s, %d\n" x i
  | FMvlo (x, i) -> Printf.fprintf oc "\tfmvlo\t%s, %d\n" x i
  | FMvhi (x, i) -> Printf.fprintf oc "\tfmvhi\t%s, %d\n" x i

  | SllI (x, y, i) -> Printf.fprintf oc "\tslli\t%s, %s, %d\n" x y i
  | SraI (x, y, i) -> Printf.fprintf oc "\tsrai\t%s, %s, %d\n" x y i
  | IMovF (x, y) -> Printf.fprintf oc "\timovf\t%s, %s\n" x y
  | FMovI (x, y) -> Printf.fprintf oc "\tfmovi\t%s, %s\n" x y

  | FMov  (x, y) -> Printf.fprintf oc "\tfmov\t%s, %s\n" x y
  | FNeg  (x, y) -> Printf.fprintf oc "\tfneg\t%s, %s\n" x y
  | FAdd (x, y, z) -> Printf.fprintf oc "\tfadd\t%s, %s, %s\n" x y z
  | FSub (x, y, z) -> Printf.fprintf oc "\tfsub\t%s, %s, %s\n" x y z
  | FMul (x, y, z) -> Printf.fprintf oc "\tfmul\t%s, %s, %s\n" x y z
  | FMulN (x, y, z) -> Printf.fprintf oc "\tfmuln\t%s, %s, %s\n" x y z
  | FInv  (x, y) -> Printf.fprintf oc "\tfinv\t%s, %s\n" x y
  | FSqrt (x, y) -> Printf.fprintf oc "\tfsqrt\t%s, %s\n" x y

  | LdI (x, y, i) -> Printf.fprintf oc "\tldi\t%s, %s, %d\n" x y i
  | StI (x, y, i) -> Printf.fprintf oc "\tsti\t%s, %s, %d\n" x y i
  | LdR (x, y, z) -> Printf.fprintf oc "\tldr\t%s, %s, %s\n" x y z
  | FLdI (x, y, i) -> Printf.fprintf oc "\tfldi\t%s, %s, %d\n" x y i
  | FStI (x, y, i) -> Printf.fprintf oc "\tfsti\t%s, %s, %d\n" x y i
  | FLdR (x, y, z) -> Printf.fprintf oc "\tfldr\t%s, %s, %s\n" x y z
	
  | BEq (x, y, l) -> Printf.fprintf oc "\tbeq\t%s, %s, %s\n" x y l
  | BLT (x, y, l) -> Printf.fprintf oc "\tblt\t%s, %s, %s\n" x y l
  | BLE (x, y, l) -> Printf.fprintf oc "\tble\t%s, %s, %s\n" x y l
  | FBEq (x, y, l) -> Printf.fprintf oc "\tfbeq\t%s, %s, %s\n" x y l
  | FBLT (x, y, l) -> Printf.fprintf oc "\tfblt\t%s, %s, %s\n" x y l
  | FBLE (x, y, l) -> Printf.fprintf oc "\tfble\t%s, %s, %s\n" x y l
	
  | J l -> Printf.fprintf oc "\tj\t%s\n" l
  | Jr x -> Printf.fprintf oc "\tjr\t%s\n" x
  | Call x -> Printf.fprintf oc "\tcall\t%s\n" x
  | CallR x -> Printf.fprintf oc "\tcallr\t%s\n" x
  | Return -> Printf.fprintf oc "\treturn\n"
  | Inputb x -> Printf.fprintf oc "\tinputb\t%s\n" x
  | Outputb x -> Printf.fprintf oc "\toutputb\t%s\n" x
  | Halt -> Printf.fprintf oc "\thalt\n"

let f oc = List.iter (o oc)
