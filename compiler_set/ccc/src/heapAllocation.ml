open Util

type 'a exp =
  | Mov            of 'a
  | Const          of Syntax.const_value
  | And            of 'a * 'a
  | Or             of 'a * 'a
  | Add            of 'a * 'a
  | Sub            of 'a * 'a
  | Negate         of 'a
  | LoadHeap       of 'a
  | LoadHeapImm    of int
    deriving (Show)

type variable =
    Variable of Id.t * Syntax.type_class * Syntax.const_value
    deriving (Show)

type instruction =
  | Label        of Id.l
  | Assignment   of Id.t * Id.t exp
  | CallAndSet   of Id.t * Id.l * Id.t list      (* with variable binding *)
  | Call         of Id.l * Id.t list      (* just calling *)
  | Definition   of variable
  | BranchZero   of Id.t * Id.l
  | BranchEqual  of Id.t * Id.t * Id.l
  | BranchLT     of Id.t * Id.t * Id.l
  | Goto         of Id.l
  | Return       of Id.t
  | ReturnVoid
  | StoreHeap    of Id.t * Id.t
  | StoreHeapImm of Id.t * int
    deriving (Show)

type t = { functions : (Syntax.function_signature * instruction list) list;
           initialize_code : instruction list }
      deriving (Show)

(* local types *)

module M = ExtendedMap.Make (Id.VStruct)

(* memory management *)
type memory_area = { mutable allocation : int M.t; mutable size : int }
let heap = { allocation = M.empty; size = 0 }

(* return from convert_exp *)
type converted_exp = Exp of Id.t exp | Insts of instruction list * Id.t exp

let find_heap var =
  try
    M.find var heap.allocation
  with
    | Not_found -> failwith ((Show.show<Id.v> var) ^ " is not found in heap.")


(* expand exp to Exp or Insts, set of insts and exp *)
let expand_exp var constructor =
  let insert_assignment temp_id ass =
    (match constructor temp_id with
        | Exp(exp) ->
          Insts([ass], exp)
        | Insts(prep, exp) ->
          Insts(ass :: prep, exp))
  in
  match var with
    | Id.V(name) -> constructor name
    | Id.A(name) ->                     (* Set and return pointer, only for call *)
      let temp_id = Id.unique "load" in
      insert_assignment temp_id (Assignment(temp_id, Const(Syntax.IntVal(find_heap var))))
    | Id.G(name) ->
      let temp_id = Id.unique "load" in
      insert_assignment temp_id (Assignment(temp_id, LoadHeapImm(find_heap var)))

let convert_exp exp =
  match exp with
    | Flow.Mov(var) ->
      (match var with
        | Id.V(name) ->
          Exp(Mov(name))
        | Id.A(name) ->
          Exp(Const(Syntax.IntVal(find_heap var)))
        | Id.G(name) ->
          Exp(LoadHeapImm(find_heap var)))
    | Flow.Const(c) -> Exp(Const(c))
    | Flow.And(var1, var2) ->
      expand_exp var1 (fun n1 ->
        expand_exp var2 (fun n2 ->
          Exp(And(n1, n2))))
    | Flow.Or(var1, var2) ->
      expand_exp var1 (fun n1 ->
        expand_exp var2 (fun n2 ->
          Exp(Or(n1, n2))))
    | Flow.Add(var1, var2) ->
      expand_exp var1 (fun n1 ->
        expand_exp var2 (fun n2 ->
          Exp(Add(n1, n2))))
    | Flow.Sub(var1, var2) ->
      expand_exp var1 (fun n1 ->
        expand_exp var2 (fun n2 ->
          Exp(Sub(n1, n2))))
    | Flow.Negate(var) ->
      expand_exp var (fun n ->
        Exp(Negate(n)))
    | Flow.ArrayGet(id, var) ->
      expand_exp var (fun name ->
        let temp_id = Id.unique "addr" in
        Insts([Assignment(temp_id, Const(Syntax.IntVal(find_heap id)));
               Assignment(temp_id, Add(temp_id, name))],
              LoadHeap(temp_id)))

let generate_assignment var =
  match var with
    | Id.V(name) -> None
    | Id.A(name) ->                     (* Set and return pointer, only for call *)
      let temp_id = Id.unique "load" in
      Some(temp_id, Assignment(temp_id, Const(Syntax.IntVal(find_heap var))))
    | Id.G(name) ->
      let temp_id = Id.unique "load" in
      Some(temp_id, Assignment(temp_id, LoadHeapImm(M.find var heap.allocation)))

let insert_load var constructor =
  match generate_assignment var with
    | Some(temp_id, ass) ->
      ass :: constructor temp_id
    | None ->
      constructor (Id.raw var)

let insert_store var constructor =
  match var with
    | Id.V(name) -> constructor name
    | Id.A(name) -> failwith "Assignment to array is not allowed"
    | Id.G(name) ->
      let temp_id = Id.unique "store" in
      constructor temp_id @ [StoreHeapImm(temp_id, M.find var heap.allocation)]

let insert_binds arg (names, assignments) =
  match generate_assignment arg with
    | None -> (Id.raw arg :: names, assignments)
    | Some(temp_id, ass) -> (temp_id :: names, ass :: assignments)

let convert_instruction = function
  | Flow.Assignment(var, exp) ->
    (match convert_exp exp with
      | Exp(exp) ->
        insert_store var (fun name -> [Assignment(name, exp)])
      | Insts(insts, last_exp) ->
        insert_store var (fun name -> insts @ [Assignment(name, last_exp)]))
  | Flow.CallAndSet(to_set, label, args) ->
    let (names, arg_assignments) = List.fold_right insert_binds args ([], []) in
    insert_store to_set (fun to_set_name ->
      arg_assignments @ [CallAndSet(to_set_name, label, names)])
  | Flow.Call(label, args) ->
    let (names, arg_assignments) = List.fold_right insert_binds args ([], []) in
    arg_assignments @ [Call(label, names)]
  | Flow.BranchZero(var, l) ->
    insert_load var (fun name -> [BranchZero(name, l)])
  | Flow.BranchEqual(var1, var2, l) ->
    insert_load var1 (fun name1 ->
      insert_load var2 (fun name2 ->
        [BranchEqual(name1, name2, l)]))
  | Flow.BranchLT(var1, var2, l) ->
    insert_load var1 (fun name1 ->
      insert_load var2 (fun name2 ->
        [BranchLT(name1, name2, l)]))
  | Flow.Return(var) ->
    insert_load var (fun name -> [Return(name)])
  | Flow.ArraySet(array, index, value) ->
    let temp = Id.unique "addr" in
    insert_load index (fun index_name ->
      insert_load value (fun value_name ->
        [Assignment(temp, Const(Syntax.IntVal(find_heap array)));
         Assignment(temp, Add(temp, index_name));
         StoreHeap(value_name, temp)]))

  | Flow.Definition(Syntax.Variable(id, typ, const)) -> [Definition(Variable(Id.raw id, typ, const))]
  | Flow.Label(l)      -> [Label(l)]
  | Flow.Goto(l)       -> [Goto(l)]
  | Flow.ReturnVoid    -> [ReturnVoid]

let temp_name = Id.unique "pointer"

let assign_global { functions = funs; initialize_code = code } t =
  let assign_and_save value location =
    [Assignment(temp_name, Const(value));
     StoreHeapImm(temp_name, location)]
  in
  let rec push_zeros top size =
    concat_map (fun i -> assign_and_save (Syntax.IntVal 0) i) (range top (top + size - 1))
  in
  match t with
    | Flow.Function(signature, insts) ->
      let new_insts = concat_map convert_instruction insts in
      { functions = funs @ [(signature, new_insts)]; initialize_code = code }
    | Flow.GlobalVariable(Syntax.Variable(id, typ, initial)) ->
      let top = heap.size in
      heap.size <- heap.size + 1;
      heap.allocation <- M.add id top heap.allocation;
      { functions = funs; initialize_code = code @ assign_and_save initial top }
    | Flow.Array({ Syntax.id = id; Syntax.size = size; _ }) ->
      let top = heap.size in
      heap.size <- heap.size + size;
      heap.allocation <- M.add id top heap.allocation;
      { functions = funs; initialize_code = code @ push_zeros top size }

let convert ts =
  let result = List.fold_left assign_global { functions = []; initialize_code = []} ts in
  print_endline (Show.show<t> result);
  result
