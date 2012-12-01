open Flow
open Util

(* Live variable analysis, per function *)

module LiveMap =
  Map.Make
    (struct
      type t = Flow.instruction_entity
      let compare = compare
     end)

let calculate_next live def use =
  S.union (S.diff live (S.of_option def)) (S.of_list use)

let use_exp = function
  | Mov(i) -> [i]
  | And(id1, id2) | Or(id1, id2)
  | Add(id1, id2) | Sub(id1, id2) -> [id1; id2]
  | Negate(id1) -> [id1]
  | Const(_) -> []

let use_instruction (E(_, inst)) = match inst with
  | Assignment(id, exp) ->
    use_exp exp
  | Call(l, args) -> args
  | CallAndSet(_, _, args) -> args
  | BranchZero(id, _) -> [id]
  | BranchEqual(id1, id2, _) | BranchLT(id1, id2, _) -> [id1; id2]
  | Return(id) -> [id]
  | _ -> []

let def_instruction (E(_, inst)) = match inst with
  | Assignment(id, _) -> Some(id)
  | Definition(Syntax.Define(id, _, _)) -> Some(id)
  | CallAndSet(id, _, _) -> Some(id)
  | _ -> None

type sucessor = Next | Jump of Id.l

let successors (E(_, inst)) = match inst with
  | BranchZero (_, l) | BranchEqual (_, _, l) | BranchLT (_, _, l) ->
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

let rec live_instruction inst (env, next, context) =
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

let live_t = function
  | Function(_, instructions) ->
    (* to compare data structure *)
    let unpack env =
      (List.map (fun (x, y) -> (x, S.elements y))(LiveMap.bindings env))
    in
    let rec loop last_env =
      let (env, _, _) = List.fold_right live_instruction instructions (last_env, None, instructions) in
      if unpack env = unpack last_env then
        (print_endline (Show.show<(Flow.instruction_entity * Id.v list) list> (unpack env));
        last_env)
      else
        loop env
    in
    loop LiveMap.empty
  | GlobalVariable(v) -> LiveMap.empty
