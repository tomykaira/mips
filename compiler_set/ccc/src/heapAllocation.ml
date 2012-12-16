open Definition
open Util

type 'a exp =
  | Mov            of 'a
  | Const          of Definition.const_value
  | And            of 'a * 'a
  | Or             of 'a * 'a
  | Add            of 'a * 'a
  | Sub            of 'a * 'a
  | Sll            of 'a * int
  | Sra            of 'a * int
  | Negate         of 'a
  | LoadHeap       of 'a
  | LoadHeapImm    of int
    deriving (Show)

type variable =
    Variable of Id.t * Definition.type_class * Definition.const_value
    deriving (Show)

type instruction =
  | Label        of Id.l
  | Assignment   of Id.t * Id.t exp
  | CallAndSet   of Id.t * Id.l * Id.t list      (* with variable binding *)
  | Call         of Id.l * Id.t list      (* just calling *)
  | BranchZero   of Id.t * Id.l
  | BranchEq     of Id.t * Id.t * Id.l
  | BranchLt     of Id.t * Id.t * Id.l
  | Goto         of Id.l
  | Return       of Id.t
  | ReturnVoid
  | StoreHeap    of Id.t * Id.t
  | StoreHeapImm of Id.t * int
    deriving (Show)

type t = { functions : (Id.v Definition.function_signature * instruction list) list;
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
    | Id.A(_) ->                     (* Set and return pointer, only for call *)
      let temp_id = Id.unique "load" in
      insert_assignment temp_id (Assignment(temp_id, Const(Definition.IntVal(find_heap var))))
    | Id.G(_) ->
      let temp_id = Id.unique "load" in
      insert_assignment temp_id (Assignment(temp_id, LoadHeapImm(find_heap var)))

let convert_exp exp =
  match exp with
    | Flow.Mov(var) ->
      (match var with
        | Id.V(name) ->
          Exp(Mov(name))
        | Id.A(_) ->
          Exp(Const(Definition.IntVal(find_heap var)))
        | Id.G(_) ->
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
    | Flow.Sll(var, i) ->
      expand_exp var (fun n ->
        Exp(Sll(n, i)))
    | Flow.Sra(var, i) ->
      expand_exp var (fun n ->
        Exp(Sra(n, i)))
    | Flow.Negate(var) ->
      expand_exp var (fun n ->
        Exp(Negate(n)))
    | Flow.ArrayGet((Id.A(_)) as id, var) ->
      expand_exp var (fun name ->
        let temp_id = Id.unique "addr" in
        Insts([Assignment(temp_id, Const(Definition.IntVal(find_heap id)));
               Assignment(temp_id, Add(temp_id, name))],
              LoadHeap(temp_id)))
    | Flow.ArrayGet(id, var) ->
      expand_exp id (fun id_name ->
        expand_exp var (fun var_name ->
          let temp_id = Id.unique "addr" in
          Insts([Assignment(temp_id, Add(id_name, var_name))],
                LoadHeap(temp_id))))

let generate_assignment var =
  match var with
    | Id.V(_) -> None
    | Id.A(_) ->                     (* Set and return pointer, only for call *)
      let temp_id = Id.unique "load" in
      Some(temp_id, Assignment(temp_id, Const(Definition.IntVal(find_heap var))))
    | Id.G(_) ->
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
    | Id.A(_) -> failwith "Assignment to array is not allowed"
    | Id.G(_) ->
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
  | Flow.BranchEq(var1, var2, l) ->
    insert_load var1 (fun name1 ->
      insert_load var2 (fun name2 ->
        [BranchEq(name1, name2, l)]))
  | Flow.BranchLt(var1, var2, l) ->
    insert_load var1 (fun name1 ->
      insert_load var2 (fun name2 ->
        [BranchLt(name1, name2, l)]))
  | Flow.Return(var) ->
    insert_load var (fun name -> [Return(name)])
  | Flow.ArraySet((Id.A(_)) as array, index, value) ->
    let temp = Id.unique "addr" in
    insert_load index (fun index_name ->
      insert_load value (fun value_name ->
        [Assignment(temp, Const(Definition.IntVal(find_heap array)));
         Assignment(temp, Add(temp, index_name));
         StoreHeap(value_name, temp)]))
  | Flow.ArraySet(pointer, index, value) ->
    let temp = Id.unique "addr" in
    insert_load pointer (fun pointer_name ->
      insert_load index (fun index_name ->
        insert_load value (fun value_name ->
          [Assignment(temp, Add(pointer_name, index_name));
           StoreHeap(value_name, temp)])))


  | Flow.Label(l)      -> [Label(l)]
  | Flow.Goto(l)       -> [Goto(l)]
  | Flow.ReturnVoid    -> [ReturnVoid]

let assign_global { functions = funs; initialize_code = code } t =
  let assign_and_save value location =
    let var = Id.unique "value" in
    [Assignment(var, Const(value));
     StoreHeapImm(var, location)]
  in
  let push_zeros top size =
    let pointer = Id.unique "start" in
    let size_register = Id.unique "size" in
    let initial_value = Id.unique "init" in
    [Assignment(pointer, Const(IntVal(top)));
     Assignment(size_register, Const(IntVal(size)));
     Assignment(initial_value, Const(IntVal(0)));
     Call(Id.L "initialize_array", [pointer; size_register; initial_value])]
  in
  let assign_string top size string =
    let chars = BatString.to_list string in
    let temp_var = Id.unique "temp" in
    if List.length chars > size then
      failwith (Printf.sprintf "Given string \"%s\" is too long for %d bytes" string size)
    else
      let string_setter = List.concat
        (List.mapi (fun i c -> [Assignment(temp_var, Const(CharVal(c))); StoreHeapImm(temp_var, top + i)]) chars) in
      if size > List.length chars then
        string_setter @ push_zeros (top + List.length chars) (size - List.length chars)
      else
        string_setter
  in
  match t with
    | Flow.Function(signature, insts) ->
      let new_insts = concat_map convert_instruction insts in
      { functions = funs @ [(signature, new_insts)]; initialize_code = code }
    | Flow.GlobalVariable(Definition.Variable(id, _, initial)) ->
      let top = heap.size in
      heap.size <- heap.size + 1;
      heap.allocation <- M.add id top heap.allocation;
      { functions = funs; initialize_code = code @ assign_and_save initial top }
    | Flow.Array({ Definition.id = id; Definition.size = size; initial = initial; _ }) ->
      let top = heap.size in
      heap.size <- heap.size + size;
      heap.allocation <- M.add id top heap.allocation;
      let initialize_code =
        match initial with
          | String(str) -> assign_string top size str
          | Zero -> push_zeros top size
          | Keep -> []
      in
      { functions = funs; initialize_code = code @ initialize_code }

let convert ts =
  let result = List.fold_left assign_global { functions = []; initialize_code = []} ts in
  Printf.eprintf "%d" (heap.size);
  print_endline (Show.show<t> result);
  result
