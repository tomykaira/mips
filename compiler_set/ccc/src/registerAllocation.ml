open LiveAnalyzer
open Util
module Heap = HeapAllocation

type instruction =
  | Assignment   of Reg.i * Reg.i Heap.exp
  | BranchZero   of Reg.i * Id.l
  | BranchEqual  of Reg.i * Reg.i * Id.l
  | BranchLT     of Reg.i * Reg.i * Id.l
  | Call         of Id.l * Reg.i list * (Reg.i * Id.t) list
  | CallAndSet   of Reg.i * Id.l * Reg.i list * (Reg.i * Id.t) list
  | Spill        of Reg.i * Id.t
  | Restore      of Reg.i * Id.t
  | Label        of Id.l
  | Return
  | Goto         of Id.l
  | StoreHeap    of Reg.i * Reg.i
  | StoreHeapImm of Reg.i * int
    deriving (Show)

type t = { functions : (Id.l * instruction list) list;
           initialize_code : instruction list }
      deriving (Show)


(* local types *)

module M = ExtendedMap.Make (Id.TStruct)

module S = ExtendedSet.Make (Id.TStruct)

module RegS = Reg.RegS

module MoveS = ExtendedSet.Make
  (struct
    type t = (Id.t * Id.t)
    let compare = compare
   end)



type register_allocation = (Id.t * Reg.i) list

let precolored         = ref []         (* variables, preassigned a register *)
let simplify_worklist  = ref S.empty    (* list of low-degree non-move-related nodes *)
let freeze_worklist    = ref S.empty    (* low-degree move-related nodes *)
let spill_worklist     = ref S.empty    (* high-degree nodes *)
let spilled_nodes      = ref S.empty    (* nodes marked for spilling during this round *)
let coalesced_map      = ref []         (* coalesced variables, (u, v) means v should be replaced with u *)
let move_list          = ref M.empty    (* list of moves *)
let interference_edges = ref []         (* interference graph represented by list of edges *)
let select_stack       = ref []         (* stack containing removed variables *)
let worklist_moves     = ref MoveS.empty (* moves currently working on *)
let active_moves       = ref MoveS.empty
let frozen_moves       = ref MoveS.empty

let reset _ =
  precolored         := [];
  simplify_worklist  := S.empty;
  freeze_worklist    := S.empty;
  spill_worklist     := S.empty;
  spilled_nodes      := S.empty;
  coalesced_map      := [];
  move_list          := M.empty;
  interference_edges := [];
  select_stack       := [];
  active_moves       := MoveS.empty;
  worklist_moves     := MoveS.empty;
  frozen_moves       := MoveS.empty


let add_move node move =
  let to_add = if M.mem node !move_list then
      MoveS.add move (M.find node !move_list)
    else
      MoveS.singleton move
  in
  move_list := M.add node to_add !move_list

let register_move = function
  | Entity.E(_, Heap.Assignment(to_node, Heap.Mov(from_node))) as inst ->
    let move = (to_node, from_node) in
    worklist_moves := MoveS.add move !worklist_moves;
    List.iter (fun id -> add_move id move) (use_instruction inst @ option_to_list (def_instruction inst))
  | _ -> ()

let add_edge u v =
  if u != v && not (List.mem (u, v) !interference_edges) then
    interference_edges := (u, v) :: (v, u) :: !interference_edges
  else
    ()

let construct_graph live inst =
  let live_here = match inst with
    | Entity.E(_, Heap.Assignment(_, Heap.Mov(_))) ->
      S.diff (LiveMap.find inst live) (S.of_list (use_instruction inst))
    | inst ->
      LiveMap.find inst live
  in
  List.iter (fun d ->
    S.iter (fun l ->
      add_edge l d) live_here) (option_to_list (def_instruction inst))

let setup_for_function live insts =
  worklist_moves := MoveS.empty;
  interference_edges := [];
  move_list := M.empty;
  List.iter (fun inst -> construct_graph live inst; register_move inst) insts



let adjacent_nodes v =
  let nodes = List.map snd (List.filter (fun (self, other) -> v = self) !interference_edges) in
  S.diff (S.of_list nodes) (S.union (S.of_list !select_stack) (S.of_list (List.map snd !coalesced_map)))

let is_significant node =
  S.cardinal (adjacent_nodes node) < Reg.available_count

(* just moved significant to not-significant *)
let just_not_significant node =
  S.cardinal (adjacent_nodes node) = Reg.available_count - 1

let node_moves v =
  try
    MoveS.inter (M.find v !move_list) (MoveS.union !active_moves !worklist_moves)
  with
    | Not_found -> MoveS.empty

let move_related v =
  not (MoveS.is_empty (node_moves v))

let make_worklist not_colored_nodes =
  let for_each_node v =
    if is_significant v then
      spill_worklist := S.add v !spill_worklist
    else if move_related v then
      freeze_worklist := S.add v !freeze_worklist
    else
      simplify_worklist := S.add v !simplify_worklist
  in
  spill_worklist    := S.empty;
  freeze_worklist   := S.empty;
  simplify_worklist := S.empty;
  S.iter for_each_node not_colored_nodes


let push_select_stack node =
  select_stack := node :: !select_stack

let enable_moves node_set =
  let update_moves move =
    if MoveS.mem move !active_moves then begin
      active_moves := MoveS.remove move !active_moves;
      worklist_moves := MoveS.add move !worklist_moves
    end else
      ()
  in
  S.iter (fun node -> MoveS.iter update_moves (node_moves node)) node_set

let remove_edges node =
  let (updated, removed) = List.partition (fun (u, v) -> u != node && v != node) !interference_edges in
  interference_edges := updated;
  let neighbors = List.map (fun (u, v) -> if u = node then v else u) removed in
  (* corresponds to DecrementDegree *)
  let update_worklist node =
    if just_not_significant node then begin
      enable_moves (S.add node (adjacent_nodes node));
      spill_worklist := S.remove node !spill_worklist;
      if move_related node then
        freeze_worklist := S.add node !freeze_worklist
      else
        simplify_worklist := S.add node !simplify_worklist
    end else ()
  in
  List.iter update_worklist neighbors

let simplify () =
  let (node, new_worklist) = S.pop !simplify_worklist in
  simplify_worklist := new_worklist;
  push_select_stack node;
  remove_edges node

let precolored_nodes () =
  S.of_list (List.map (fun (n, r) -> n) !precolored)

let is_precolored node =
  List.exists (fun (n, r) -> n = node) !precolored

let prepare_simplify node =
  if not (is_precolored node) && not (move_related node) && not (is_significant node) then begin
    freeze_worklist := S.remove node !freeze_worklist;
    simplify_worklist := S.add node !simplify_worklist
  end else
    ()

let is_coalescable_conservative nodes =
  S.cardinal (S.filter is_significant nodes) < Reg.available_count

let resolve_alias node =
  try
    rev_assoc node !coalesced_map
  with
    | Not_found -> node

let combine (u, v) =
  freeze_worklist := S.remove v !freeze_worklist;
  spill_worklist := S.remove v !spill_worklist;
  coalesced_map := (u, v) :: !coalesced_map;
  S.iter (add_edge u) (adjacent_nodes v);
  remove_edges v;
  if is_significant u && S.mem u !freeze_worklist then begin
    freeze_worklist := S.remove u !freeze_worklist;
    spill_worklist := S.add u !spill_worklist
  end else
    ()

let is_adjacent (u, v) =
  List.mem (u, v) !interference_edges

let coalesce () =
  let ((x, y), new_worklist) = MoveS.pop !worklist_moves in
  let (x, y) = (resolve_alias x, resolve_alias y) in
  let (u, v) = if is_precolored y then (y, x) else (x, y) in
  worklist_moves := new_worklist;
  if u = v then begin
    prepare_simplify u
  end else if is_precolored v || is_adjacent (u, v) then begin
    prepare_simplify u;
    prepare_simplify v;
  (* in my data structure, no need to use different algorithm for precolored *)
  end else if is_coalescable_conservative (S.union (adjacent_nodes u) (adjacent_nodes v)) then begin
        combine (u, v);
        prepare_simplify u
      end else
      active_moves := MoveS.add (x, y) !active_moves


let freeze_moves node =
  let inner ((x, y) as move) =
    active_moves := MoveS.remove move !active_moves;
    frozen_moves := MoveS.add move !frozen_moves;
    let other_node =
      if resolve_alias y = resolve_alias node then
        resolve_alias x
      else
        resolve_alias y
    in
    if MoveS.is_empty (node_moves other_node) && not (is_significant other_node) then begin
      freeze_worklist := S.remove other_node !freeze_worklist;
      simplify_worklist := S.add other_node !simplify_worklist
    end else
      ()
  in
  (List.iter inner $ MoveS.elements $ node_moves) node

let freeze () =
  let (node, new_worklist) = S.pop !freeze_worklist in
  freeze_worklist := new_worklist;
  simplify_worklist := S.add node !simplify_worklist;
  freeze_moves node



let select_spill () =
  (* TODO: better spill selection algorithm *)
  let (node, new_worklist) = S.pop !spill_worklist in
  spill_worklist := new_worklist;
  simplify_worklist := S.add node !simplify_worklist;
  freeze_moves node

let assign_colors () =
  (* It must be from top to bottom *)
  let colored_nodes = ref [] in
  let color_node _ node =
    let available = ref Reg.available_registers in
    let remove_filled node =
      let actual = resolve_alias node in
      try
        let (var, reg) = List.find (fun (v, r) -> v = actual) (!colored_nodes @ !precolored) in
        available := RegS.remove reg !available
      with
        | Not_found -> ()
    in
    S.iter remove_filled (adjacent_nodes node);
    if RegS.is_empty !available then
      spilled_nodes := S.add node !spilled_nodes
    else begin
      let (reg, _) = RegS.pop !available in
      colored_nodes := (node, reg) :: !colored_nodes
    end
  in
  List.fold_left color_node () !select_stack;
  List.iter (fun (name_to, name_from) ->
    colored_nodes := (name_from, List.assoc name_to !colored_nodes) :: !colored_nodes) !coalesced_map;
  !colored_nodes

let rewrite_program _ _ =
  failwith "Oh sorry, you cannot retry."

let replace_registers live colored_nodes insts =
  let r id = List.assoc id colored_nodes in
  let replace_exp = function
    | Heap.Mov(id)          -> Heap.Mov(r id)
    | Heap.Const(c)         -> Heap.Const(c)
    | Heap.And(id1, id2)    -> Heap.And(r id1, r id2)
    | Heap.Or(id1, id2)     -> Heap.Or(r id1, r id2)
    | Heap.Add(id1, id2)    -> Heap.Add(r id1, r id2)
    | Heap.Sub(id1, id2)    -> Heap.Sub(r id1, r id2)
    | Heap.Negate(id1)      -> Heap.Negate(r id1)
    | Heap.LoadHeap(id1)    -> Heap.LoadHeap(r id1)
    | Heap.LoadHeapImm(int) -> Heap.LoadHeapImm(int)

  in
  let replace (Entity.E(_, inst) as identified) =
    match inst with
      | Heap.Assignment(id, exp) ->
        [Assignment(r id, replace_exp exp)]
      | Heap.Call(l, args) ->
        let live = LiveMap.find identified live in
        let allocation = S.map_list (fun name -> (r name, name)) live in
        [Call(l, List.map r args, allocation)]
      | Heap.CallAndSet(to_set, l, args) ->
        let live = LiveMap.find identified live in
        let allocation = S.map_list (fun name -> (r name, name)) (S.remove to_set live) in
        [CallAndSet(r to_set, l, List.map r args, allocation)]
      | Heap.BranchZero(id, l) ->
        [BranchZero(r id, l)]
      | Heap.BranchEqual(id1, id2, l) ->
        [BranchEqual(r id1, r id2, l)]
      | Heap.BranchLT(id1, id2, l) ->
        [BranchLT(r id1, r id2, l)]
      | Heap.Return(id) ->
        if r id = Reg.ret then
          [Return]
        else
          failwith "return address is not return register"
      | Heap.StoreHeap(id1, id2) ->
        [StoreHeap(r id1, r id2)]
      | Heap.StoreHeapImm(id1, imm) ->
        [StoreHeapImm(r id1, imm)]

      | Heap.ReturnVoid    -> [Return]
      | Heap.Label(l)      -> [Label(l)]
      | Heap.Goto(l)       -> [Goto(l)]
      | Heap.Definition(_) -> []
  in
  concat_map replace insts

let rec retry insts =
  color_variables (rewrite_program !spilled_nodes insts)
and color_variables insts =
  reset ();
  let other_nodes = S.diff (extract_nodes insts) (precolored_nodes ()) in
  let live = live_t insts in
  setup_for_function live insts;
  make_worklist other_nodes;
  while S.is_empty !simplify_worklist && MoveS.is_empty !worklist_moves &&
    S.is_empty !freeze_worklist && S.is_empty !spill_worklist do
    if S.not_empty !simplify_worklist then
      simplify ()
    else if MoveS.not_empty !worklist_moves then
      coalesce ()
    else if S.not_empty !freeze_worklist then
      freeze ()
    else if S.not_empty !spill_worklist then
      select_spill ()
    else
      ()
  done;
  let colored_nodes = assign_colors () in
  if S.is_empty !spilled_nodes then
    replace_registers live colored_nodes insts
  else
    retry insts

let insert_precolored_variables insts =
  let create_assignment (old_id, new_id, _) =
    Heap.Assignment(new_id, Heap.Mov(old_id))
  in
  let register_binding (_, new_id, num) =
    (new_id, Reg.(`I num))
  in
  let get_new (_, n, _) = n in
  let replace inst (accumulate, pairs) = match inst with
    | Heap.Return (t) ->
      let id = Id.unique "return" in
      (Heap.Assignment(id, Heap.Mov(t)) :: Heap.Return(id) :: accumulate,
       (id, Reg.ret) :: pairs)
    | Heap.CallAndSet (old_id, l, args) ->
      let mapping = List.mapi (fun num old_id -> (old_id, Id.unique "arg", num + 3)) args in
      let return_id = Id.unique "receiver" in
      (List.map create_assignment mapping
       @ [Heap.CallAndSet(return_id, l, List.map get_new mapping);
          Heap.Assignment(old_id, Heap.Mov(return_id))]
       @ accumulate,
       (return_id, Reg.ret) :: List.map register_binding mapping @ pairs)
    | Heap.Call (l, args) ->
      let mapping = List.mapi (fun num old_id -> (old_id, Id.unique "arg", num + 3)) args in
      (List.map create_assignment mapping @ [Heap.Call(l, List.map get_new mapping)] @ accumulate,
       List.map register_binding mapping @ pairs)
    | inst ->
      (inst :: accumulate, pairs)
  in
  List.fold_right replace insts ([], [])

let get_abi_constraint { Syntax.parameters = params; _ } : (Id.t * Reg.i) list =
  Reg.assign_params (List.map (Id.raw $ Syntax.parameter_id) params)

let convert_function ({Syntax.name = name} as signature, insts) =
  let (insts, return_precoloring) = insert_precolored_variables insts in
  let parameter_precoloring = get_abi_constraint signature in
  precolored := parameter_precoloring @ return_precoloring;
  let identified_insts = Entity.identify insts in
  (name, color_variables identified_insts)

let convert { Heap.functions = funs; Heap.initialize_code = init } =
  { functions = List.map convert_function funs; initialize_code = color_variables (Entity.identify init) }
