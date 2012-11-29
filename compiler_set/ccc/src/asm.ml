(* Directly corresponds to assembly *)
type label = Id.l

type t =
  | Exec of statement
  | AssignInt   of Reg.i   * int_exp
  | AssignFloat of Reg.f * float_exp
  | Label of label

and int_exp =
  | Int of int
  | SETL of label

  | ADD of Reg.i * Reg.i
  | SUB of Reg.i * Reg.i
  | XOR of Reg.i * Reg.i

  | ADDI of Reg.i * int
  | SUBI of Reg.i * int
  | XORI of Reg.i * int

  | SLLI of Reg.i * int
  | SRAI of Reg.i * int

  | FMOVI of Reg.f

  | LDI of Reg.i * int
  | LDR of Reg.i * Reg.i

and float_exp =
  | Float of float

  | IMOVF of Reg.i

  | FADD  of Reg.f * Reg.f
  | FSUB  of Reg.f * Reg.f
  | FMUL  of Reg.f * Reg.f
  | FINV  of Reg.f
  | FSQRT of Reg.f

  | FLDI of Reg.i * int
  | FLDR of Reg.i * Reg.i

and statement =
  | NOP
  | BEQ  of Reg.i * Reg.i * label
  | BLT  of Reg.i * Reg.i * label
  | BLE  of Reg.i * Reg.i * label
  | FBEQ of Reg.f * Reg.f * label
  | FBLT of Reg.f * Reg.f * label
  | FBLE of Reg.f * Reg.f * label
  | CALL of label
  | CALLR of Reg.i
  | STI   of Reg.i * Reg.i * int
  | FSTI  of Reg.i * Reg.f * int

let rec print_stat out_channel stat =
  let print1 inst arg =
    Printf.fprintf out_channel "\t%s\t%s" inst arg in
  let print_branch inst reg1 reg2 (Id.L label) =
    Printf.fprintf out_channel "\t%s\t%s, %s, %s" inst (Reg.show reg1) (Reg.show reg2) label in
  let print_save inst reg1 reg2 off =
    Printf.fprintf out_channel "\t%s\t%s, %s, %s" inst (Reg.show reg1) (Reg.show reg2) (string_of_int off) in
  match stat with
    | NOP ->
      output_string out_channel "nop"

    | BEQ(reg1, reg2, l) ->
      print_branch "beq" reg1 reg2 l
    | BLT(reg1, reg2, l) ->
      print_branch "blt" reg1 reg2 l
    | BLE(reg1, reg2, l) ->
      print_branch "ble" reg1 reg2 l
    | FBEQ(reg1, reg2, l) ->
      print_branch "fbeq" reg1 reg2 l
    | FBLT(reg1, reg2, l) ->
      print_branch "fblt" reg1 reg2 l
    | FBLE(reg1, reg2, l) ->
      print_branch "fble" reg1 reg2 l

    | CALL(Id.L l) ->
      print1 "call" l
    | CALLR(reg) ->
      print1 "callr" (Reg.show reg)

    | STI(reg1, reg2, off) ->
      print_save "sti" reg1 reg2 off
    | FSTI(reg1, reg2, off) ->
      print_save "fsti" reg1 reg2 off

let rec print_int_exp out_channel destination exp =
  let print1 inst arg = Printf.fprintf out_channel "\t%s\t%s, %s" inst (Reg.show destination) arg in
  let print2 inst arg1 arg2 = Printf.fprintf out_channel "\t%s\t%s, %s, %s" inst (Reg.show destination) arg1 arg2 in
  match exp with
  | Int(i) when i > 0x7fff ->
    let j = Int32.to_int (Int32.shift_right_logical (Int32.of_int i) 15) in
    let j = if j > 0x7FFF then j land 0x7FFF else j in
    print2 "addi" (Reg.int_zero) (string_of_int j);
    print2 "slli" (Reg.show destination) "15";
    print2 "addi" (Reg.show destination) (string_of_int (Int32.to_int (Int32.logand (Int32.of_int i) 0x7FFFl)))
  | Int(i) when -0x8000 > i ->
    print_int_exp out_channel destination (Int(-i));
    print2 "sub" (Reg.int_zero) (Reg.show destination)
  | Int(i) ->
    print2 "addi" (Reg.int_zero) (string_of_int i)

  | SETL(Id.L l) ->
    print1 "setl" l

  | ADD(reg1, reg2) ->
    print2 "add" (Reg.show reg1) (Reg.show reg2)
  | SUB(reg1, reg2) ->
    print2 "sub" (Reg.show reg1) (Reg.show reg2)
  | XOR(reg1, reg2) ->
    print2 "xor" (Reg.show reg1) (Reg.show reg2)

  | ADDI(reg, imm) ->
    print2 "addi" (Reg.show reg) (string_of_int imm)
  | SUBI(reg, imm) ->
    print2 "subi" (Reg.show reg) (string_of_int imm)
  | XORI(reg, imm) ->
    print2 "xori" (Reg.show reg) (string_of_int imm)
  | SLLI(reg, imm) ->
    print2 "slli" (Reg.show reg) (string_of_int imm)
  | SRAI(reg, imm) ->
    print2 "srai" (Reg.show reg) (string_of_int imm)

  | FMOVI(reg) ->
    print1 "fmovi" (Reg.show reg)

  | LDI(reg, imm) ->
    print2 "ldi" (Reg.show reg) (string_of_int imm)
  | LDR(reg1, reg2) ->
    print2 "ldr" (Reg.show reg1) (Reg.show reg2)

let print_float_exp out_channel destination exp =
  let print1 inst arg = Printf.fprintf out_channel "\t%s\t%s, %s" inst (Reg.show destination) arg in
  let print2 inst arg1 arg2 = Printf.fprintf out_channel "\t%s\t%s, %s, %s" inst (Reg.show destination) arg1 arg2 in
  match exp with
  | Float(i) ->
    let high = (Int32.to_int (Int32.shift_right_logical (Int32.bits_of_float i) 16)) in
    let low = Int32.to_int (Int32.logand (Int32.bits_of_float i) 0xFFFFl) in
    print1 "fmvhi" (string_of_int high);
    if low <> 0 then print1 "fmvlo" (string_of_int low)
  | IMOVF(reg) ->
    print1 "imovf" (Reg.show reg)
  | FADD(reg1, reg2) ->
    print2 "fadd" (Reg.show reg1) (Reg.show reg2)
  | FSUB(reg1, reg2) ->
    print2 "fsub" (Reg.show reg1) (Reg.show reg2)
  | FMUL(reg1, reg2) ->
    print2 "fmul" (Reg.show reg1) (Reg.show reg2)
  | FINV(reg1) ->
    print1 "finv" (Reg.show reg1)
  | FSQRT(reg1) ->
    print1 "fsqrt" (Reg.show reg1)

  | FLDI(reg, imm) ->
    print2 "fldi" (Reg.show reg) (string_of_int imm)
  | FLDR(reg1, reg2) ->
    print2 "fldr" (Reg.show reg1) (Reg.show reg2)

let print out_channel = function
  | Exec(stat) ->
    print_stat out_channel stat
  | AssignInt(destination, exp) ->
    print_int_exp out_channel destination exp
  | AssignFloat(destination, exp) ->
    print_float_exp out_channel destination exp
  | Label(Id.L label) ->
    Printf.fprintf out_channel "%s:\n" label
