(* MemoryAllocation to asm.  Its end of the tale. *)
open Util
open MemoryAllocation
open Definition
open Asm

let header =
  [Exec(NOP)]

let before_asm =
  [Exec(CALL (Id.L "main"));
   AssignInt(Reg.ret, Int(231));
   Exec(OUTPUTB Reg.ret);
   AssignInt(Reg.ret, Int(181));
   Exec(OUTPUTB Reg.ret);
   AssignInt(Reg.ret, Int(130));
   Exec(OUTPUTB Reg.ret);
   Exec(RETURN);
   Label(Id.L "asm_here")]

let end_label () =
  [Label(Id.L "invoke_subprocess");
   Exec(STI(Reg.heap_pointer, Reg.frame, 0));
   AssignInt(Reg.frame, SUBI(Reg.frame, 1));
   AssignInt(Reg.heap_pointer, Int(HeapAllocation.(heap.size)));
   Exec(CALL(Id.L "program_end"));
   AssignInt(Reg.frame, ADDI(Reg.frame, 1));
   AssignInt(Reg.heap_pointer, LDI(Reg.frame, 0));
   Exec(RETURN);
   Label(Id.L "program_end")]

let convert_exp = function
  | Mov(r)             -> ADD(Reg.int_zero, r)
  | Add(a, b)          -> ADD(a, b)
  | Sub(a, b)          -> SUB(a, b)
  | Negate(a)          -> SUB(Reg.int_zero, a)
  | Const(IntVal(i))   -> Int(i)
  | Const(CharVal(c))  -> Int(Char.code c)
  | Const(FloatVal(f)) -> failwith "Float value is not yet supported"
  | x -> failwith ("oops.. sorry, not supported: " ^ (Show.show<exp> x))


let convert_instruction = function
  | Assignment(reg, Mov(r)) when r = reg -> []
  | Assignment(reg, exp) ->
    [AssignInt(reg, convert_exp exp)]
  | BranchZero(reg, l) ->
    [Exec(BEQ(reg, Reg.int_zero, l))]
  | BranchEq(r1, r2, l) ->
    [Exec(BEQ(r1, r2, l))]
  | BranchLt(r1, r2, l) ->
    [Exec(BLT(r1, r2, l))]
  | Call (l, offset) ->
    if offset > 0 then
      [AssignInt(Reg.frame, SUBI(Reg.frame, offset));
       Exec(CALL(l));
       AssignInt(Reg.frame, ADDI(Reg.frame, offset))]
    else
      [Exec(CALL(l))]
  | Store (reg, Heap(off)) ->
    [Exec(STI(reg, Reg.heap_pointer, off))]
  | Store (reg, HeapReg(address)) ->
    [AssignInt(Reg.address, ADD(Reg.heap_pointer, address));
     Exec(STI(reg, Reg.address, 0))]
  | Store (reg, Stack(off)) ->
    [Exec(STI(reg, Reg.frame, -off))]
  | Load (reg, Heap(off)) ->
    [AssignInt(reg, LDI(Reg.heap_pointer, off))]
  | Load (reg, HeapReg(address)) ->
    [AssignInt(reg, LDR(Reg.heap_pointer, address))]
  | Load (reg, Stack(off)) ->
    [AssignInt(reg, LDI(Reg.frame, -off))]
  | MemoryAllocation.Label(l) ->
    [Label(l)]
  | Return ->
    [Exec(RETURN)]
  | Goto(l) ->
    [Exec(JUMP(l))]


let convert_function (name, code) =
  Label(name) :: concat_map convert_instruction code

let convert { functions = funs; initialize_code = code } =
  header @ concat_map convert_instruction code @ before_asm @ concat_map convert_function funs @ end_label ()
