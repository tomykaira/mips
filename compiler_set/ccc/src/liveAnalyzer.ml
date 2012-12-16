open HeapAllocation
open Entity
open Util

(* Live variable analysis, per function *)

module LiveMap =
  Map.Make
    (struct
      type t = HeapAllocation.instruction Entity.entity
      let compare = compare
     end)

module S = ExtendedSet.Make(Id.TStruct)

let calculate_next live def use =
  S.union (S.diff live (S.of_list def)) (S.of_list use)

let use_exp = function
  | Mov(i)
  | LoadHeap(i)
  | Sll(i, _)
  | Sra(i, _)
  | Negate(i) -> [i]

  | And(id1, id2)
  | Or(id1, id2)
  | Add(id1, id2)
  | Sub(id1, id2) -> [id1; id2]

  | LoadHeapImm(_)
  | Const(_) -> []

let use_instruction (E(_, inst)) = match inst with
  | Assignment(_, exp) ->
    use_exp exp
  | Call(_, args) -> args
  | CallAndSet(_, _, args) -> args
  | BranchZero(id, _) -> [id]
  | BranchEq(id1, id2, _) | BranchLt(id1, id2, _) -> [id1; id2]
  | Return(id) -> [id]
  | StoreHeap(id1, id2) -> [id1; id2]
  | StoreHeapImm(id1, _) -> [id1]
  | _ -> []

let def_instruction (E(_, inst)) = match inst with
  | Assignment(id, _) -> [id]
  | CallAndSet(id, _, _) -> [id]
  | _ -> []

type sucessor = Next | Jump of Id.l

let successors (E(_, inst)) = match inst with
  | BranchZero (_, l) | BranchEq (_, _, l) | BranchLt (_, _, l) ->
    [Next; Jump l]
  | Goto(l) -> [Jump l]
  | Return _ | ReturnVoid -> []
  | _ -> [Next]

(* Find labeled instruction in context (instruction list) *)
exception LabelNotFound of Id.l
exception NoSuccessor of Id.l

let find_label context label =
  let label_matcher = function
    | E(_, Label(l)) when l = label -> true
    | _ -> false
  in
  List.find label_matcher context

let live_instruction inst (env, next, context) =
  let find_or_empty inst =
    if LiveMap.mem inst env then
      LiveMap.find inst env
    else
      S.empty
  in
  let for_instruction = function
    | Some(succ_inst) ->
      calculate_next (find_or_empty succ_inst) (def_instruction succ_inst) (use_instruction succ_inst)
    | None -> S.empty
  in
  let find_successor = function
    | Next   -> next
    | Jump l -> Some(find_label context l)
  in
  let live = (S.unions $ List.map (for_instruction $ find_successor) $ successors) inst in
  (LiveMap.add inst live env, Some(inst), context)

let live_t instructions =
    (* to compare data structure *)
  let unpack env =
    (List.map (fun (x, y) -> (x, S.elements y))(LiveMap.bindings env))
  in
  let rec loop last_env =
    let (env, _, _) = List.fold_right live_instruction instructions (last_env, None, instructions) in
    if unpack env = unpack last_env then
      (print_endline (Show.show<(instruction entity * Id.t list) list> (unpack env));
       last_env)
    else
      loop env
  in
  loop LiveMap.empty

let extract_nodes insts =
  S.of_list (concat_map use_instruction insts @ concat_map def_instruction insts)
