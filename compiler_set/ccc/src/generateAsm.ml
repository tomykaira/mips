(* MemoryAllocation to asm.  Its end of the tale. *)
open Util
open Asm

let convert_exp = function
  | RegisterAllocation.Mov(r)             -> ADD(Reg.int_zero, r)
  | RegisterAllocation.Add(a, b)          -> ADD(a, b)
  | RegisterAllocation.Sub(a, b)          -> SUB(a, b)
  | RegisterAllocation.Negate(a)          -> SUB(Reg.int_zero, a)
  | RegisterAllocation.Const(Syntax.IntVal(i))   -> Int(i)
  | RegisterAllocation.Const(Syntax.CharVal(c))  -> Int(Char.code c)
  | RegisterAllocation.Const(Syntax.FloatVal(f)) -> failwith "Float value is not yet supported"
  | _ -> failwith "oops.. sorry, not supported"


let convert_instruction = function
  | MemoryAllocation.Assignment(reg, RegisterAllocation.Mov(r)) when r = reg -> []
  | MemoryAllocation.Assignment(reg, exp) ->
    [AssignInt(reg, convert_exp exp)]
  | MemoryAllocation.BranchZero(reg, l) ->
    [Exec(BEQ(reg, Reg.int_zero, l))]
  | MemoryAllocation.BranchEqual(r1, r2, l) ->
    [Exec(BEQ(r1, r2, l))]
  | MemoryAllocation.BranchLT(r1, r2, l) ->
    [Exec(BLT(r1, r2, l))]
  | MemoryAllocation.Call (l, offset) ->
    if offset > 0 then
      [AssignInt(Reg.frame, SUBI(Reg.frame, offset));
       Exec(CALL(l));
       AssignInt(Reg.frame, ADDI(Reg.frame, offset))]
    else
      [Exec(CALL(l))]
  | MemoryAllocation.Store (reg, MemoryAllocation.Heap(off)) ->
    [Exec(STI(reg, Reg.int_zero, off))]
  | MemoryAllocation.Store (reg, MemoryAllocation.Stack(off)) ->
    [Exec(STI(reg, Reg.frame, -off))]
  | MemoryAllocation.Load (reg, MemoryAllocation.Heap(off)) ->
    [AssignInt(reg, LDI(Reg.int_zero, off))]
  | MemoryAllocation.Load (reg, MemoryAllocation.Stack(off)) ->
    [AssignInt(reg, LDI(Reg.frame, -off))]
  | MemoryAllocation.Label(l) ->
    [Label(l)]
  | MemoryAllocation.Return ->
    [Exec(RETURN)]
  | MemoryAllocation.Goto(l) ->
    [Exec(JUMP(l))]


let convert_function (name, code) =
  Label(name) :: concat_map convert_instruction code

let convert { MemoryAllocation.functions = funs; MemoryAllocation.initialize_code = code } =
  concat_map convert_instruction code @ concat_map convert_function funs
