(* rename identifiers in Syntax tree to make them unique (alpha-conversion) *)
open Syntax
open Util

let rename_variable env (Define(name, typ, const)) =
  let new_name = Id.unique (Id.raw name) in
  (M.add name (Id.V new_name) env, Define((Id.V new_name), typ, const))

let rename_global_variable env (Define(name, typ, const)) =
  let new_name = Id.unique (Id.raw name) in
  (M.add name (Id.G new_name) env, Define((Id.G new_name), typ, const))

let rename_parameter env (Parameter(typ, name)) =
  let new_name = Id.unique (Id.raw name) in
  (M.add name (Id.V new_name) env, Parameter(typ, (Id.V new_name)))

let fold_rename_variable v (e, vs) =
  let (e', v') = rename_variable e v in
  (e', v' :: vs)

let fold_rename_parameter p (e, ps) =
  let (e', p') = rename_parameter e p in
  (e', p' :: ps)

let rec convert_exp env e =
  let go = convert_exp env in
  match e with
    | Var(v) when M.mem v env -> Var(M.find v env)
    | Var(v)              -> Var(v)                  (* function name? *)
    | Const(v)            -> Const(v)
    | Assign(e1, e2)      -> Assign(go e1, go e2)
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

    | PostIncrement(e1) -> PostIncrement(go e1)
    | PostDecrement(e1) -> PostDecrement(go e1)

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
      | Function (id, typ, params, stat) ->
        let (new_env, new_params) = List.fold_right fold_rename_parameter params (env, []) in
        let new_stat = convert_statement env stat in
        (env, Function(id, typ, new_params, new_stat) :: definitions)

      | GlobalVariable(variable) ->
        let (new_env, v) = rename_global_variable env variable in
        (new_env, GlobalVariable(v) :: definitions)

  in
  let (env, result) = List.fold_left convert_t (M.empty, []) ts in
  let result = List.rev result in
  List.iter (print_endline $ Show.show<t>) result;
  result
