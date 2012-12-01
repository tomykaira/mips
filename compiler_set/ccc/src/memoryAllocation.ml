(*
  Allocate memories
  - Place global variables
  - Insert save / restore
*)
open Util

type memory_point = Heap of int | HeapReg of Reg.i | Stack of int

type instruction =
  | Assignment  of Reg.i * RegisterAllocation.exp
  | BranchZero  of Reg.i * Id.l
  | BranchEqual of Reg.i * Reg.i * Id.l
  | BranchLT    of Reg.i * Reg.i * Id.l
  | Call        of Id.l * int
  | Store       of Reg.i * memory_point
  | Load        of Reg.i * memory_point
  | Label       of Id.l
  | Return
  | Goto        of Id.l

type t = { functions : (Id.l * instruction list) list; initialize_code : instruction list }

type memory_area = { mutable allocation : int M.t; mutable size : int }
let stack = { allocation = M.empty; size = 0 }
let heap  = { allocation = M.empty; size = 0 }

(* Use global environment with side-effect *)

(* Get address of global or array *)
let address id =
  match id with
    | Id.A(a) ->
      if M.mem id heap.allocation then
        M.find id heap.allocation
      else
        failwith ("Unknown array variable: " ^ a)
    | Id.G(g) ->
      if M.mem id heap.allocation then
        M.find id heap.allocation
      else
        failwith "Unknown global variable."
    | Id.V(_) ->
      failwith "address calculation is not permitted for stack variable"

(* Return location for given variable.  If not allocated, newly allocate it. *)
let location id =
  match id with
    | Id.V(v) ->
      if M.mem id stack.allocation then
        Stack (M.find id stack.allocation)
      else
        let s = stack.size in
        stack.size <- s + 1;
        stack.allocation <- M.add id s stack.allocation;
        Stack s
    | Id.G(_) | Id.A(_) ->
      Heap (address id)

let rec assign_local inst =
  let store_before_call = concat_map (fun (reg, id) -> assign_local (RegisterAllocation.Spill(reg, id))) in
  let load_after_call   = concat_map (fun (reg, id) -> assign_local (RegisterAllocation.Restore(reg, id))) in
  let move_args args =
    let mapping = Reg.assign_params args in
    List.map (fun (to_reg, from_reg) -> Assignment(to_reg, RegisterAllocation.Mov(from_reg))) mapping
  in
  match inst with
    | RegisterAllocation.Call(l, args, { RegisterAllocation.to_save = to_save; RegisterAllocation.to_restore = to_restore}) ->
      load_after_call to_restore
      @ [Call(l, stack.size)]
      @ move_args args
      @ store_before_call to_save

    | RegisterAllocation.CallAndSet(dest, l, args, { RegisterAllocation.to_save = to_save; RegisterAllocation.to_restore = to_restore}) ->
      load_after_call to_restore
      @ [Assignment(dest, RegisterAllocation.Mov(Reg.ret));
         Call(l, stack.size)]
      @ move_args args
      @ store_before_call to_save

    | RegisterAllocation.ArraySet(id, offset_reg, value) ->
      [Store(value, HeapReg Reg.address);
       Assignment(Reg.address, RegisterAllocation.Add(Reg.address, offset_reg));
       Assignment(Reg.address, RegisterAllocation.Const(Syntax.IntVal(address id)))]

    | RegisterAllocation.Spill(reg, id) -> [Store(reg, location id)]
    | RegisterAllocation.Restore(reg, id) -> [Load(reg, location id)]

    | RegisterAllocation.Assignment(to_reg, RegisterAllocation.ArrayGet(id, offset_reg)) ->
      [Load(to_reg, HeapReg Reg.address);
       Assignment(Reg.address, RegisterAllocation.Add(Reg.address, offset_reg));
       Assignment(Reg.address, RegisterAllocation.Const(Syntax.IntVal(address id)))]

    | RegisterAllocation.Assignment(r, exp)     -> [Assignment(r, exp)]
    | RegisterAllocation.BranchZero(r, l)       -> [BranchZero(r, l)]
    | RegisterAllocation.BranchEqual(r1, r2, l) -> [BranchEqual(r1, r2, l)]
    | RegisterAllocation.BranchLT(r1, r2, l)    -> [BranchLT(r1, r2, l)]
    | RegisterAllocation.Label(l)               -> [Label(l)]
    | RegisterAllocation.Return                 -> [Return]
    | RegisterAllocation.Goto(l)                -> [Goto(l)]

let assign_global { functions = funs; initialize_code = code } t =
  let assign_and_save value location =
    [Assignment(Reg.ret, RegisterAllocation.Const(value));
     Store(Reg.ret, Heap location)]
  in
  let rec push_zeros top size =
    if size = 0 then
      []
    else
      Store(Reg.int_zero, Heap top) :: push_zeros (top + 1) (size - 1)
  in
  match t with
    | RegisterAllocation.Function(id, insts) ->
      stack.size <- 0;
      stack.allocation <- M.empty;
      let converted = (id, (List.rev $ List.concat) (List.fold_left (fun acc i -> assign_local i :: acc) [] insts)) in
      { functions = funs @ [converted]; initialize_code = code }
    | RegisterAllocation.GlobalVariable(Syntax.Variable(id, typ, initial)) ->
      let top = heap.size in
      heap.size <- heap.size + 1;
      heap.allocation <- M.add id top heap.allocation;
      { functions = funs; initialize_code = code @ assign_and_save initial top }
    | RegisterAllocation.Array({ Syntax.id = id; Syntax.size = size; _ }) ->
      let top = heap.size in
      heap.size <- heap.size + size;
      heap.allocation <- M.add id top heap.allocation;
      { functions = funs; initialize_code = code @ push_zeros top size }

let convert ts =
  List.fold_left assign_global { functions = []; initialize_code = []} ts
