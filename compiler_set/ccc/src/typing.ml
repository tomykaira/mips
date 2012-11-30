open Util
open Syntax

exception Unify of Type.t * Type.t
exception Undefined of Id.t
exception NotFunction of Syntax.exp * Type.t
exception NotPrimitive of Syntax.exp

type environment = { variables: Type.t M.t; functions: Type.t M.t }

let extract_bodies t =
  let body = function
    | GlobalVariable(_) -> []
    | Function(_, return_type, _, stat) -> [(convert_syntactic_type return_type, stat)]
  in
  concat_map body t

let unify typ1 typ2 =
  if typ1 = typ2 then
    ()
  else
    raise (Unify(typ1, typ2))

let rec binding (Define(name, typ, const_value)) =
    let typ = convert_syntactic_type typ in
    unify (typ) (const_type const_value);
    (name, typ)

let rec get_exp_type env exp =
  let go = get_exp_type env in
  let assert_primitive e =
    match go e with
      | Type.Int | Type.Char | Type.Float -> ()
      | _ -> raise (NotPrimitive(e))
  in
  let find_function f =
    let { functions = fs; _ } = env in
    if M.mem f fs then
      Some(M.find f fs)
    else
      None
  in
  let find_variable v =
    let { variables = vs; _ } = env in
    if M.mem v vs then
      Some(M.find v vs)
    else
      None
  in
  match exp with
  | Var(id) ->
    (match find_variable id with
      | Some(t) -> t
      | None -> raise (Undefined(id)))
  | Const(value) -> const_type value
  | Assign(e1, e2) ->
    let t2 = go e2 in
    unify (go e1) t2;
    t2
  | And(e1, e2) | Or(e1, e2)
  | Equal(e1, e2) | LessThan(e1, e2) | GreaterThan(e1, e2) ->
    assert_primitive e1; assert_primitive e2; Type.Int
  | Add(e1, e2) | Sub(e1, e2) | Mul(e1, e2) | Div(e1, e2) | Mod(e1, e2) ->
    let t1 = go e1 in
    assert_primitive e1; assert_primitive e2;
    t1
  | Not(e) -> assert_primitive e; Type.Int
  | Negate(e) ->
    assert_primitive e;
    go e
  | CallFunction(Id.L f, args) ->
    let arg_types = List.map go args in
    (match find_function f with
      | Some(Type.Fun(ret_type, param_types)) ->
        List.iter2 unify param_types arg_types;
        ret_type
      | Some(_) -> failwith "unreachable"
      | None -> raise (Undefined(f))
    )
  | PostIncrement(e) | PostDecrement(e) ->
    assert_primitive e;
    go e

(* get_exp_type will throw exception, if some error found *)
let rec check_exp env exp =
  ignore (get_exp_type env exp); ()

let rec check_statement env return_type stat =
  let go = check_statement env return_type in
  let go_exp = check_exp env in
  match stat with
    | Label(_, stat) -> go stat
    | Exp(exp) -> go_exp exp
    | Block(variables, stats) ->
      let { variables = vs; functions = fs } = env in
      let new_env = { variables = M.add_list (List.map binding variables) vs; functions = fs } in
      List.iter (check_statement new_env return_type) stats
    | If(exp, stat_true, Some(stat_false)) ->
      go_exp exp;
      go stat_true;
      go stat_false
    | If(exp, stat_true, None) ->
      go_exp exp;
      go stat_true
    | Switch(exp, cases) ->
      (
        let exp_type = get_exp_type env exp in
        let check_case_statement = function
          | SwitchCase(const, stat) ->
            unify exp_type (const_type const);
            go stat
          | DefaultCase(stat) ->
            go stat
        in
        List.iter check_case_statement cases
      )
    | While(exp, stat) ->
      go_exp exp;
      go stat
    | Return (Some(exp)) ->
      let typ = get_exp_type env exp in
      unify return_type typ
    | Return (None) ->
      unify Type.Void return_type
    | _ -> ()

let check_top t {variables = vs; functions = fs} =
  match t with
    | Function(Id.L id, return_type, params, stat) ->
      let param_type (Parameter (typ, _)) = typ in
      let return_type = convert_syntactic_type return_type in
      let fun_typ = Type.Fun (return_type,
                              List.map (convert_syntactic_type $ param_type) params) in
      let new_env = { variables = vs; functions = M.add id fun_typ fs } in
      check_statement new_env return_type stat;
      new_env
    | GlobalVariable(var) ->
      { variables = M.add_pair (binding var) vs; functions = fs }

let type_check t =
  List.fold_right (check_top) t {variables = M.empty; functions = M.empty}
