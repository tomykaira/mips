open LiveAnalyzer
open HeapAllocation
open Util

type instruction_entity = instruction Entity.entity
    deriving (Show)

type node = Inst of instruction_entity | Dummy
    deriving (Show)

module NodeStruct = struct
  type t = node
  let compare = compare
end

module IdM = ExtendedMap.Make(Id.TStruct)
module InstM = ExtendedMap.Make(NodeStruct)
module S = ExtendedSet.Make(NodeStruct)

type edge = { source : node; destination : node;
              latency : int; priority : int; selected : bool }
    deriving (Show)

let exp_latency = function
  | LoadHeap(_)
  | LoadHeapImm(_) -> 1
  | _ -> 0

let instruction_latency (Entity.E(_, inst)) = match inst with
  | StoreHeap(_)
  | StoreHeapImm(_) -> 1
  | Assignment(id, exp) ->
    exp_latency exp

  | _ -> 0

(* split at non-schedulable instruction.  PC and memory control instructions cannot move*)
let schedulable (Entity.E(_, inst)) = match inst with
  | Label(_)
  | CallAndSet(_)
  | Call(_)
  | BranchZero(_)
  | BranchEq(_)
  | BranchLt(_)
  | Goto(_)
  | Return(_)
  | StoreHeap(_)
  | StoreHeapImm(_)
  | ReturnVoid -> false
  | _ -> true

let construct_dependency_graph insts =
  let insts = List.filter schedulable insts in
  let find_setter setter id =
    try
      Some(IdM.find id setter)
    with
      | Not_found -> None               (* depends on a variable from out of block *)
  in
  let predecessor_priority edges dependent =
    try
      (List.find (fun edge -> edge.destination = dependent) edges).priority
    with
      | Not_found -> 0
  in
  (* read instruction from top to down, add dependency edges.
     be cautious about WAW and WAR hazards *)
  let add_edge (edges, setter, roots) inst =
    let related = use_instruction inst @ def_instruction inst in
    let dependents = concat_option (List.map (find_setter setter) related) in
    let new_setter = IdM.add_list (List.map (fun w -> (w, Inst inst)) (def_instruction inst)) setter in
    let create_edge dependent =
      { source = dependent; destination = Inst inst;
        latency = instruction_latency inst;
        priority = predecessor_priority edges dependent + 1;
        selected = false }
    in
    (List.map (fun d -> create_edge d) dependents @ edges,
     new_setter,
     S.add (Inst inst) (S.diff roots (S.of_list (dependents))))
  in
  (* tie root nodes to Dummy node.  This dummy behaves as the end of block *)
  let root_to_dummy edges root =
    { source = root;
      destination = Dummy;
      latency = 0;
      priority = predecessor_priority edges root + 1;
      selected = false }
  in
  let (edges, _, roots) = List.fold_left add_edge ([], IdM.empty, S.empty) insts in
  List.map (root_to_dummy edges) (S.elements roots) @ edges

let ready_set graph  =
  let (sources, destinations) = List.split (List.map (fun edge -> (edge.source, edge.destination)) graph) in
  S.remove Dummy (S.diff (S.of_list sources) (S.of_list destinations))

(* select critical path *)
let calculate_priority_critical_path graph ready_insts =
  let rec route_cost inst =
    match inst with
      | Inst(_) ->
        let { latency = t; destination = d; _ } = List.find (fun edge -> edge.source = inst) graph in
        let (next_cost, root) = route_cost d in
        (next_cost + t, root)
      | Dummy ->                        (* dummy is root *)
        (0, inst)
  in
  let max (inst1, cost1) (inst2, cost2) =
    if cost1 > cost2 then
      (inst1, cost1)
    else
      (inst2, cost2)
  in
  let update inst root_map =
    let (cost, root) = (route_cost inst) in
    if InstM.mem root root_map then
      InstM.add root (max (inst, cost) (InstM.find root root_map)) root_map
    else
      InstM.add root (inst, cost) root_map
  in
  let mid_result = List.fold_right update ready_insts InstM.empty in
  List.hd (List.sort (fun (_, (_, cost1)) (_, (_, cost2)) -> compare cost1 cost2) (InstM.bindings mid_result))

let select graph ready_insts =
  let (root, (selected, cost)) = calculate_priority_critical_path graph (S.elements ready_insts) in
  match selected with
    | Inst(inst) -> inst
    | Dummy -> failwith "Dummy should not selected"

(* count down latency of the instructions whose source is already selected *)
let tick_without_selection graph =
  List.fold_right
    (fun ({ selected = selected; latency = t; source = s; _} as edge) others ->
      if selected && t = 0 then
        others
      else if selected && t > 0 then
        { edge with latency = t - 1 } :: others
      else
        edge :: others)
    graph []

let tick graph selected_node =
  let edge_replaced = List.map
    (fun ({ source = s; _} as edge) ->
      if s = Inst selected_node then
        { edge with selected = true; source = Dummy }
      else
        edge)
    graph
  in
  tick_without_selection edge_replaced

let convert_block block =
  let graph = ref (construct_dependency_graph block) in
  let scheduled = ref [] in

  while List.length !graph > 0 do
    let ready_insts = ready_set !graph in
    graph := if S.is_empty ready_insts then
        tick_without_selection !graph
      else
        (let selected = select !graph ready_insts in
         scheduled := selected :: !scheduled;
         tick !graph selected)
  done;
  List.rev !scheduled

let convert_function insts =
  let insts = Entity.identify insts in
  let fold inst (current, other) =
    if schedulable inst then
      (inst :: current, other)
    else
      ([], inst :: (convert_block current) @ other)
  in
  let (rest, chunks) = List.fold_right fold insts ([], []) in
  Entity.unidentify (convert_block rest @ chunks)

let convert { functions = functions; initialize_code = initialize_code} =
  let result = { functions = List.map (fun (signature, insts) -> (signature, convert_function insts)) functions;
                 initialize_code = initialize_code} in
  print_endline (Show.show<t> result);
  result
