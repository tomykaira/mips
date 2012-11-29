open Util
open Syntax

exception Unify of Type.t * Type.t
exception Undefined of Id.t
exception NotFunction of Syntax.exp * Type.t
exception NotPrimitive of Syntax.exp

let extract_bodies t =
  let body = function
    | GlobalVariable(_) -> []
    | Function(_, return_type, _, stat) -> [(convert_syntactic_type return_type, stat)]
  in
  List.concat (List.map body t)

let unify typ1 typ2 =
  if typ1 = typ2 then
    ()
  else
    raise (Unify(typ1, typ2))

let rec binding (Define(name, typ, const_value)) =
    let typ = convert_syntactic_type typ in
    unify (typ) (const_type const_value);
    (name, typ)

let binding_top = function
  | Function(name, return_type, parameters, _) ->
    let extract_parameter_type (Parameter (typ, _)) = typ in
    (name, Type.Fun (convert_syntactic_type return_type, List.map (convert_syntactic_type $ extract_parameter_type) parameters))
  | GlobalVariable(var) ->
    binding var

let rec get_exp_type env exp =
  let go = get_exp_type env in
  let assert_primitive e =
    match go e with
      | Type.Int | Type.Char | Type.Float -> ()
      | _ -> raise (NotPrimitive(e))
  in
  match exp with
  | Var(id) ->
    if M.mem id env then M.find id env else raise (Undefined(id))
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
  | CallFunction(f, args) ->
    let arg_types = List.map go args in
    let fun_type = go f in
    (match fun_type with
      | Type.Fun(ret_type, param_types) ->
        List.iter2 unify param_types arg_types;
        ret_type
      | _ -> raise (NotFunction(f, fun_type)))
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
      let new_env = M.add_list (List.map binding variables) env in
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

let type_check t =
  let contexts = extract_bodies t in
  let env = M.from_list (List.map binding_top t) in
  List.iter (uncurry2 (check_statement env)) contexts
