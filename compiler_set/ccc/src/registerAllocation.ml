open Util

type exp =
  | Mov            of Reg.i
  | Const          of Syntax.const_value
  | And            of Reg.i * Reg.i
  | Or             of Reg.i * Reg.i
  | Equal          of Reg.i * Reg.i
  | LessThan       of Reg.i * Reg.i
  | GreaterThan    of Reg.i * Reg.i
  | Add            of Reg.i * Reg.i
  | Sub            of Reg.i * Reg.i
  | Mul            of Reg.i * Reg.i
  | Div            of Reg.i * Reg.i
  | Mod            of Reg.i * Reg.i
  | Not            of Reg.i
  | Negate         of Reg.i

type instruction =
  | Assignment of Reg.i * exp
  | BranchZero of Reg.i * Id.l
  | BranchEqual of Reg.i * Reg.i * Id.l
  | Call of Id.l * Reg.i list * (Reg.i * Id.t) list
  | Spill of Reg.i * Id.t
  | Restore of Reg.i * Id.t
  | Label of Id.l
  | Return
  | Goto of Id.l

type t =
  | Function of Id.l * instruction list
  | GlobalVariable of Syntax.variable

let rev_assoc value xs =
  fst (List.find (fun (k, v) -> v = value) xs)

let spilled_arguments spilled inst =
  let use = LiveAnalyzer.use_instruction inst in
  S.elements (S.inter spilled (S.of_list use))

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

let allocate live usage to_allocate =
  let live_usage = List.filter (fun (reg, var) -> S.exists ((=) var) live) usage in
  let not_used = Reg.rest (List.map fst live_usage) in
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
    | FlatExp.Var(v)                -> Mov(r v)
    | FlatExp.Const(const)          -> Const(const)
    | FlatExp.And(id1, id2)         -> And(r id1, r id2)
    | FlatExp.Or(id1, id2)          -> Or(r id1, r id2)
    | FlatExp.Equal(id1, id2)       -> Equal(r id1, r id2)
    | FlatExp.LessThan(id1, id2)    -> LessThan(r id1, r id2)
    | FlatExp.GreaterThan(id1, id2) -> GreaterThan(r id1, r id2)
    | FlatExp.Add(id1, id2)         -> Add(r id1, r id2)
    | FlatExp.Sub(id1, id2)         -> Sub(r id1, r id2)
    | FlatExp.Mul(id1, id2)         -> Mul(r id1, r id2)
    | FlatExp.Div(id1, id2)         -> Div(r id1, r id2)
    | FlatExp.Mod(id1, id2)         -> Mod(r id1, r id2)
    | FlatExp.Not(id)               -> Not(r id)
    | FlatExp.Negate(id)            -> Negate(r id)
    | FlatExp.CallFunction _ -> failwith "not comes here"

let replace allocation live inst =
  let reg_of v = rev_assoc v allocation in
  let regs_of = List.map reg_of in
  let to_save =
    List.map (fun var -> List.find (fun (_, v) -> v = var) allocation) (S.elements live)
  in
  match inst with
  | Flow.Assignment(id, FlatExp.CallFunction(l, args)) ->
    [Assignment(reg_of id, Mov(Reg.ret));
     Call(l, regs_of args, to_save);]

  | Flow.Assignment(id, exp) ->
    [Assignment(reg_of id, replace_exp allocation exp)]

  | Flow.Call(l, args) ->
    [Call(l, regs_of args, to_save)]

  | Flow.Definition(Syntax.Define(id, typ, init)) ->
    [Assignment(reg_of id, Const(init))]

  | Flow.BranchZero(id, l) ->
    [BranchZero(reg_of id, l)]

  | Flow.BranchEqual(id1, id2, l) ->
    [BranchEqual(reg_of id1, reg_of id2, l)]

  | Flow.Return(id) ->
    let reg = rev_assoc id allocation in
    [Return; Assignment(Reg.ret, Mov(reg))]

  | Flow.Label(l) -> [Label(l)]
  | Flow.Goto(l) -> [Goto(l)]
  | Flow.ReturnVoid -> [Return]

type replacement_context = {live : S.t LiveAnalyzer.LiveMap.t; usage : (Reg.i * Id.t) list; spilled : S.t}

let update_usage usage live =
  List.filter (fun (_, id) -> S.mem id live) usage

let replace_variables ({live = live; usage = usage; spilled = spilled}, new_insts) inst =
  let this_living              = LiveAnalyzer.LiveMap.find inst live in
  let to_restore               = spilled_arguments spilled inst in
  let to_assign                = new_assignment usage inst in
  let (new_alloc, to_spill)    = allocate this_living usage (to_assign @ to_restore) in
  let restore_insts            = List.map (restore_instruction new_alloc) to_restore in
  let new_inst                 = replace (new_alloc @ usage) this_living inst in
  let new_usage                = update_usage (new_alloc @ usage) this_living in
  let spill_insts              = List.map spill_instruction to_spill in
  let spilled_vars             = S.of_list (List.map snd to_spill) in
  ({live = live;
    usage = new_usage;
    spilled = S.union spilled_vars spilled},
   new_inst @ restore_insts @ spill_insts @ new_insts)

(* Initialize context with function parameters *)
let initialize f params =
  let usage = Reg.assign_params (List.map (fun (Syntax.Parameter(_, id)) -> id) params) in
  { live = LiveAnalyzer.live_t f; usage = usage; spilled = S.empty }

let convert_top = function
  | (Flow.Function(id, typ, params, insts) as f) ->
    let env = initialize f params in
    let insts = snd (List.fold_left replace_variables (env, []) insts) in
    Function(id, insts)
  | Flow.GlobalVariable(v) ->
    GlobalVariable(v)

let convert ts =
  List.map convert_top ts
