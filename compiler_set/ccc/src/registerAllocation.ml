open Util

type exp =
  | Mov            of Reg.i
  | Const          of Syntax.const_value
  | And            of Reg.i * Reg.i
  | Or             of Reg.i * Reg.i
  | Add            of Reg.i * Reg.i
  | Sub            of Reg.i * Reg.i
  | Negate         of Reg.i
  | ArrayGet       of Id.v * Reg.i      (* TODO: force array by types *)
    deriving (Show)

type call_context = { to_save: (Reg.i * Id.v) list; to_restore: (Reg.i * Id.v) list }
    deriving (Show)

type instruction =
  | Assignment  of Reg.i * exp
  | BranchZero  of Reg.i * Id.l
  | BranchEqual of Reg.i * Reg.i * Id.l
  | BranchLT    of Reg.i * Reg.i * Id.l
  | Call        of Id.l * Reg.i list * call_context
  | CallAndSet  of Reg.i * Id.l * Reg.i list * call_context
  | Spill       of Reg.i * Id.v
  | Restore     of Reg.i * Id.v
  | Label       of Id.l
  | Return
  | Goto        of Id.l
  | ArraySet    of Id.v * Reg.i * Reg.i
    deriving (Show)

type t =
  | Function of Id.l * instruction list
  | GlobalVariable of Syntax.variable
  | Array of Syntax.array_signature
    deriving (Show)

let rev_assoc value xs =
  try
    fst (List.find (fun (k, v) -> v = value) xs)
  with
      Not_found ->
        failwith (Printf.sprintf "%s is not found in allocation list %s"
                    (Show.show<Id.v> value) (Show.show<(Reg.i * Id.v) list> xs))

let spilled_arguments spilled allocation inst =
  let use = LiveAnalyzer.use_instruction inst in
  let allocated = List.map snd allocation in
  S.elements (S.inter spilled (S.diff (S.of_list use) (S.of_list allocated)))

let is_allocated alloc id =
  List.exists (fun (_, v) -> id = v) alloc

let new_assignment usage inst =
  match LiveAnalyzer.def_instruction inst with
    | None -> []
    | Some(id) ->
      if is_allocated usage id then
        []
      else
        [id]

let spill_LRU usage count =
  let to_spill = BatList.take count (List.rev usage) in
  (List.map fst to_spill, to_spill)

let using_registers live usage =
  let live_usage = List.filter (fun (reg, var) -> S.exists ((=) var) live) usage in
  List.map fst live_usage

let allocate live usage to_allocate =
  let not_used = Reg.rest (using_registers live usage) in
  let available = List.length not_used in
  let required = List.length to_allocate in
  if available >= required then
    (zip not_used to_allocate, [])
  else
    let (freed, to_spill) = spill_LRU usage (required - available) in
    (zip (not_used @ freed) to_allocate, to_spill)

let restore_instruction allocation id =
  let reg = rev_assoc id allocation in
  Restore(reg, id)

let spill_instruction (reg, id) =
  Spill(reg, id)

let replace_exp allocation exp =
  let r v = rev_assoc v allocation in
  match exp with
    | Flow.Mov(v)                -> Mov(r v)
    | Flow.Const(const)          -> Const(const)
    | Flow.And(id1, id2)         -> And(r id1, r id2)
    | Flow.Or(id1, id2)          -> Or(r id1, r id2)
    | Flow.Add(id1, id2)         -> Add(r id1, r id2)
    | Flow.Sub(id1, id2)         -> Sub(r id1, r id2)
    | Flow.Negate(id)            -> Negate(r id)
    | Flow.ArrayGet(array, id)   -> ArrayGet(array, r id)

let write_back_global reg id =
  match id with
    | Id.V(_) -> []
    | Id.A(_) -> []
    | Id.G(_) -> [Spill(reg, id)]

let replace allocation call_context (Flow.E(_, inst)) =
  let reg_of v = rev_assoc v allocation in
  let regs_of = List.map reg_of in
  let remove_assignee ({ to_save = s; to_restore = r}) id =
    let remove = List.filter (fun (_, i) -> id != i) in
    { to_save = remove s; to_restore = remove r}
  in
  match inst with
  | Flow.Assignment(id, exp) ->
    write_back_global (reg_of id) id @ [Assignment(reg_of id, replace_exp allocation exp)]

  | Flow.Call(l, args) ->
    [Call(l, regs_of args, call_context)]

  | Flow.CallAndSet(id, l, args) ->
    write_back_global (reg_of id) id @
      [CallAndSet(reg_of id, l, regs_of args, remove_assignee call_context id)]

  | Flow.Definition(Syntax.Variable(id, typ, init)) ->
    [Assignment(reg_of id, Const(init))]

  | Flow.BranchZero(id, l) ->
    [BranchZero(reg_of id, l)]

  | Flow.BranchEqual(id1, id2, l) ->
    [BranchEqual(reg_of id1, reg_of id2, l)]

  | Flow.BranchLT(id1, id2, l) ->
    [BranchLT(reg_of id1, reg_of id2, l)]

  | Flow.Return(id) ->
    let reg = rev_assoc id allocation in
    [Return; Assignment(Reg.ret, Mov(reg))]

  | Flow.ArraySet(id, index, value) ->
    [ArraySet(id, reg_of index, reg_of value)]

  | Flow.Label(l) -> [Label(l)]
  | Flow.Goto(l) -> [Goto(l)]
  | Flow.ReturnVoid -> [Return]

type replacement_context = {live : S.t LiveAnalyzer.LiveMap.t; usage : (Reg.i * Id.v) list; spilled : S.t}

let update_usage usage live =
  List.filter (fun (_, id) -> S.mem id live) usage

let replace_variables ({live = live; usage = usage; spilled = spilled}, new_insts) inst =
  let to_restore               = spilled_arguments spilled usage inst in
  let to_assign                = new_assignment usage inst in

  print_endline (Show.show<Id.v list> to_restore);
  print_endline (Show.show<Id.v list> to_assign);

  let this_living              = LiveAnalyzer.LiveMap.find inst live in
  let (new_alloc, to_spill)    = allocate this_living usage (to_assign @ to_restore) in
  let new_usage                = update_usage (new_alloc @ usage) this_living in

  print_endline (Show.show<Id.v list> (S.elements this_living));
  print_endline (Show.show<(Reg.i * Id.v) list> (new_alloc @ usage));

  (* registers to be saved and restored *)
  let call_context =
    let inter usage new_usage =
      List.filter (fun (reg, id) -> List.exists (fun (r, i) -> i = id) usage) new_usage
    in
    let f u = concat_map (fun var -> List.find_all (fun (_, v) -> v = var) u) (S.elements this_living) in
    { to_save = f usage; to_restore = f (inter usage new_usage) } (* restore variables stored, and used afterward *)
  in

  (* newly allocated registers are available, and registers die here are still allocated *)
  let new_inst                 = replace (new_alloc @ usage) call_context inst in
  let spill_insts              = List.map spill_instruction to_spill in
  let restore_insts            = List.map (restore_instruction new_alloc) to_restore in

  let spilled_vars             = S.of_list (List.map snd to_spill) in

  Printf.printf "%s\t%s\t%s\n" (Show.show<instruction list> new_inst)
    (Show.show<(Reg.i * Id.v) list> usage)
    (Show.show<(Reg.i * Id.v) list> new_usage);
  ({live = live;
    usage = new_usage;
    spilled = S.union spilled_vars spilled},
   new_inst @ restore_insts @ spill_insts @ new_insts)

(* Initialize context with function parameters *)
let initialize f params heap_variables =
  let usage = Reg.assign_params (List.map Syntax.parameter_id params) in
  { live = LiveAnalyzer.live_t f; usage = usage; spilled = S.of_list heap_variables }

let convert_top (result, heap_variables) = function
  | (Flow.Function({Syntax.name = name; Syntax.parameters = params; _}, insts) as f) ->
    let env = initialize f params heap_variables in
    let insts = snd (List.fold_left replace_variables (env, []) insts) in
    (Function(name, List.rev insts) :: result, heap_variables)
  | Flow.GlobalVariable(Syntax.Variable(id, _, _) as v) ->
    (GlobalVariable(v) :: result, id :: heap_variables)
  | Flow.Array(array_sig) ->
    (Array(array_sig) :: result, heap_variables)

let convert ts =
  let (result, _) = List.fold_left convert_top ([], []) ts in
  let result = List.rev result in
  print_endline (Show.show<t list> result);
  result
