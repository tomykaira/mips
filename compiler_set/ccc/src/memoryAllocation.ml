(*
  Allocate memories
  - Insert save / restore
*)
open Definition
open Util

type memory_point = Heap of int | HeapReg of Reg.i | Stack of int

type exp =
  | Mov            of Reg.i
  | Const          of const_value
  | And            of Reg.i * Reg.i
  | Or             of Reg.i * Reg.i
  | Add            of Reg.i * Reg.i
  | Sub            of Reg.i * Reg.i
  | Negate         of Reg.i
    deriving (Show)

type instruction =
  | Assignment  of Reg.i * exp
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


(* local types *)

module Heap = HeapAllocation
module RegAlloc = RegisterAllocation
module M = ExtendedMap.Make (Id.TStruct)

(* Use global environment with side-effect *)
type memory_area = { mutable allocation : int M.t; mutable size : int }
let stack = { allocation = M.empty; size = 0 }


(* Return location for given variable.  If not allocated, newly allocate it. *)
let location id =
  if M.mem id stack.allocation then
    Stack (M.find id stack.allocation)
  else
    let s = stack.size in
    stack.size <- s + 1;
    stack.allocation <- M.add id s stack.allocation;
    Stack s

let move_exp = function
  | Heap.Mov(reg)         -> Mov(reg)
  | Heap.Const(c)         -> Const(c)
  | Heap.And(reg1, reg2)  -> And(reg1, reg2)
  | Heap.Or(reg1, reg2)   -> Or(reg1, reg2)
  | Heap.Add(reg1, reg2)  -> Add(reg1, reg2)
  | Heap.Sub(reg1, reg2)  -> Sub(reg1, reg2)
  | Heap.Negate(reg1)     -> Negate(reg1)
  | Heap.LoadHeap(reg1)   -> assert false
  | Heap.LoadHeapImm(int) -> assert false

let rec assign_local inst =
  let store_before_call = concat_map (fun (reg, id) -> assign_local (RegAlloc.Spill(reg, id))) in
  let load_after_call   = concat_map (fun (reg, id) -> assign_local (RegAlloc.Restore(reg, id))) in
  match inst with
    | RegAlloc.Call(l, args, to_save) ->
      load_after_call to_save
      @ [Call(l, stack.size)]
      @ store_before_call to_save

    | RegAlloc.CallAndSet(dest, l, args, to_save) ->
      load_after_call to_save
      @ [Assignment(dest, Mov(Reg.ret));
         Call(l, stack.size)]
      @ store_before_call to_save

    | RegAlloc.StoreHeap(value, index) ->
      [Store(value, HeapReg index)]
    | RegAlloc.StoreHeapImm(value, offset) ->
      [Store(value, Heap offset)]

    | RegAlloc.Spill(reg, id) -> [Store(reg, location id)]
    | RegAlloc.Restore(reg, id) -> [Load(reg, location id)]

    | RegAlloc.Assignment(to_reg, Heap.LoadHeap(offset_reg)) ->
      [Load(to_reg, HeapReg offset_reg)]
    | RegAlloc.Assignment(to_reg, Heap.LoadHeapImm(offset)) ->
      [Load(to_reg, Heap offset)]

    | RegAlloc.Assignment(r, exp)     -> [Assignment(r, move_exp exp)]
    | RegAlloc.BranchZero(r, l)       -> [BranchZero(r, l)]
    | RegAlloc.BranchEqual(r1, r2, l) -> [BranchEqual(r1, r2, l)]
    | RegAlloc.BranchLT(r1, r2, l)    -> [BranchLT(r1, r2, l)]
    | RegAlloc.Label(l)               -> [Label(l)]
    | RegAlloc.Return                 -> [Return]
    | RegAlloc.Goto(l)                -> [Goto(l)]

let insert_stack_operation (id, insts) =
  stack.size <- 0;
  stack.allocation <- M.empty;
  (* process order is important *)
  (id, (List.rev $ List.concat) (List.fold_left (fun acc i -> assign_local i :: acc) [] insts))

let convert { RegAlloc.functions = funs; RegAlloc.initialize_code = init } =
  let new_functions = List.map insert_stack_operation funs in
  { functions = new_functions; initialize_code = concat_map assign_local init }
