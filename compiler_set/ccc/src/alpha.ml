(* rename identifiers in Syntax tree to make them unique (alpha-conversion) *)
open Syntax
open Util

module M = ExtendedMap.Make (Id.VStruct)

let rename_variable env (Variable(name, typ, const)) =
  let new_name = Id.unique (Id.raw name) in
  (M.add name (Id.V new_name) env, Variable((Id.V new_name), typ, const))

let rename_global_variable env (Variable(name, typ, const)) =
  let new_name = Id.unique (Id.raw name) in
  (M.add name (Id.G new_name) env, Variable((Id.G new_name), typ, const))

(* Map Id.V old_name -> Id.A new_name
   To replace Id.v occurrence in statements *)
let rename_array env ({id = id; } as signature) =
  let raw = Id.raw id in
  let new_name = Id.A(Id.unique raw) in
  let variable_map = ((Id.V raw), new_name) in (* FIXME *)
  let array_map = ((Id.A raw), new_name) in
  (M.add_list [variable_map; array_map] env, { signature with id = new_name})

let rename_parameter env = function
  | Parameter(typ, name) ->
    let new_name = Id.unique (Id.raw name) in
    (M.add name (Id.V new_name) env, Parameter(typ, (Id.V new_name)))
  | PointerParameter(typ, name) ->
    let new_name = Id.unique (Id.raw name) in
    (M.add name (Id.V new_name) env, PointerParameter(typ, (Id.V new_name)))

let fold_rename_variable v (e, vs) =
  let (e', v') = rename_variable e v in
  (e', v' :: vs)

let fold_rename_parameter p (e, ps) =
  let (e', p') = rename_parameter e p in
  (e', p' :: ps)

let rec convert_exp env e =
  let go = convert_exp env in
  let rename_assignee = function
    | VarSet(v) when M.mem v env ->
      VarSet(M.find v env)
    | ArraySet(v, exp) when M.mem v env ->
      ArraySet(M.find v env, go exp)
    | ass ->
      failwith ("You cannot set to an unknown variable " ^ (Show.show<Syntax.assignee> ass) ^ ".")
  in
  match e with
    | Var(v) when M.mem v env -> Var(M.find v env)
    | Var(v)              -> Var(v)                  (* function name? *)
    | ArrayRef(a, e) when M.mem a env -> ArrayRef(M.find a env, go e)
    | ArrayRef(a, _)      -> failwith (Printf.sprintf "Unknown array %s is referred." (Show.show<Id.v> a))
    | Const(v)            -> Const(v)
    | Assign(a, e)        -> Assign(rename_assignee a, go e)
    | And(e1, e2)         -> And(go e1, go e2)
    | Or(e1, e2)          -> Or(go e1, go e2)
    | Equal(e1, e2)       -> Equal(go e1, go e2)
    | LessThan(e1, e2)    -> LessThan(go e1, go e2)
    | GreaterThan(e1, e2) -> GreaterThan(go e1, go e2)
    | Add(e1, e2)         -> Add(go e1, go e2)
    | Sub(e1, e2)         -> Sub(go e1, go e2)
    | Mul(e1, e2)         -> Mul(go e1, go e2)
    | Div(e1, e2)         -> Div(go e1, go e2)
    | Mod(e1, e2)         -> Mod(go e1, go e2)

    | Not(e1)    -> Not(go e1)
    | Negate(e1) -> Negate(go e1)

    | CallFunction(l, args) -> CallFunction(l, List.map go args)

let rec convert_statement env stat =
  let go = convert_statement env in
  let go_exp = convert_exp env in
  match stat with
    | Label(l, stat) -> Label(l, go stat)
    | Exp(e) -> Exp(go_exp e)
    | Block(variables, statements) ->
      let (new_env, variables) = List.fold_right fold_rename_variable variables (env, []) in
      Block(variables, List.map (convert_statement new_env) statements)
    | If(e, stat_true, Some(stat_false)) ->
      If(go_exp e, go stat_true, Some(go stat_false))
    | Switch(e, cases) ->
      let new_cases =
        List.map
          (function SwitchCase(const, stat) -> SwitchCase(const, go stat)
            | DefaultCase(stat) -> DefaultCase(go stat)) cases
      in
      Switch(go_exp e, new_cases)
    | While(e, stat) ->
      While(go_exp e, go stat)
    | Return(Some(e)) ->
      Return(Some(go_exp e))
    | stat -> stat



let convert ts =
  let convert_t (env, definitions) t =
    match t with
      | MacroExpand.Function ({ name = name; return_type = return_type; parameters = params } , stat) ->
        let (new_env, new_params) = List.fold_right fold_rename_parameter params (env, []) in
        let new_stat = convert_statement new_env stat in
        (env, MacroExpand.Function({ name = name; return_type = return_type; parameters = new_params }, new_stat) :: definitions)

      | MacroExpand.FunctionDeclaration (_) ->
        (env, t :: definitions)

      | MacroExpand.GlobalVariable(variable) ->
        let (new_env, v) = rename_global_variable env variable in
        (new_env, MacroExpand.GlobalVariable(v) :: definitions)

      | MacroExpand.Array(signature) ->
        let (new_env, new_sig) = rename_array env signature in
        (new_env, MacroExpand.Array(new_sig) :: definitions)

  in
  let (env, result) = List.fold_left convert_t (M.empty, []) ts in
  let result = List.rev result in
  List.iter (print_endline $ Show.show<MacroExpand.t>) result;
  result
