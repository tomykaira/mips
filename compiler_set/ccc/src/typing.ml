open Util
open Syntax

exception Unify of Type.t * Type.t
exception UndefinedVariable of Id.v
exception UndefinedFunction of Id.l
exception NotFunction of Syntax.exp * Type.t
exception NotPrimitive of Syntax.exp
exception NotArray of Id.v

module FunM = ExtendedMap.Make (Id.LStruct)
module M = ExtendedMap.Make (Id.VStruct)

type environment = { variables: Type.t M.t; functions: Type.fun_type FunM.t }

let extract_bodies t =
  let body = function
    | MacroExpand.GlobalVariable(_) -> []
    | MacroExpand.FunctionDeclaration(_) -> []
    | MacroExpand.Array(_) -> []
    | MacroExpand.Function({return_type = return_type; _}, stat) -> [(convert_syntactic_type return_type, stat)]
  in
  concat_map body t

let unify typ1 typ2 =
  if typ1 = typ2 then
    ()
  else
    raise (Unify(typ1, typ2))

let rec binding (Variable(name, typ, const_value)) =
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
    if FunM.mem f fs then
      Some(FunM.find f fs)
    else
      None
  in
  let find_variable v =
    if M.mem v (env.variables) then
      M.find v env.variables
    else
      raise (UndefinedVariable(v))
  in
  let assignee_type = function
    | VarSet(v) -> find_variable v
    | ArraySet(a, e) ->
      match find_variable a with
        | Type.Array(t) -> t
        | _ -> raise (NotArray(a))
  in
  match exp with
  | Var(id) ->
    find_variable id
  | Const(value) -> const_type value
  | ArrayRef(id, index) ->
    unify Type.Int (go index);
    (match find_variable id with
      | Type.Array(t) -> t
      | _ -> raise (NotArray(id)))
  | Assign(assignee, e2) ->
    let t2 = go e2 in
    unify (assignee_type assignee) t2;
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
  | CallFunction(label, args) ->
    let arg_types = List.map go args in
    (match find_function label with
      | Some(Type.Fun(ret_type, param_types)) ->
        List.iter2 unify param_types arg_types;
        ret_type
      | None ->
        raise (UndefinedFunction(label))
    )

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

let check_top ({variables = vs; functions = fs} as env) t =
  let add_function_type {name = label; return_type = return_type; parameters = params} =
    let return_type = convert_syntactic_type return_type in
    let fun_typ = Type.Fun (return_type,
                            List.map parameter_type params) in
    { env with functions = FunM.add label fun_typ fs }
  in
  let add_parameters { variables = vs; functions = fs } params =
    let param_binds = List.map (fun p -> (parameter_id p, parameter_type p)) params in
    { env with variables = M.add_list param_binds vs }
  in
  match t with
    | MacroExpand.Function({return_type = return_type; parameters = params; _} as signature, stat) ->
      let new_env = add_function_type signature in
      let local_env = add_parameters new_env params in
      check_statement local_env (convert_syntactic_type return_type) stat;
      new_env
    | MacroExpand.FunctionDeclaration(signature) ->
      add_function_type signature
    | MacroExpand.GlobalVariable(var) ->
      { env with variables = M.add_pair (binding var) vs }

    (* TODO: limit array label to `A *)
    | MacroExpand.Array({id = typed_id; content_type = typ; _}) ->
      match typed_id with
        | Id.A id ->
          (* There are both Id.A and Id.V before alpha-transformation *)
          let array_label = (Id.A id,  (Type.Array (convert_syntactic_type typ))) in
          let variable_label = (Id.V id,  (Type.Array (convert_syntactic_type typ))) in
          { env with variables = M.add_list [array_label; variable_label] vs }
        | _ ->
          failwith "Unexpected id type for array"

let check ts =
  try
    ignore (List.fold_left (check_top) {variables = M.empty; functions = FunM.empty} ts);
    ts
  with
    | Unify(t1, t2) ->
      failwith (Printf.sprintf "Failed to unify types %s %s" (Show.show<Type.t> t1) (Show.show<Type.t> t2))
    | UndefinedVariable(v) ->
      failwith (Printf.sprintf "Undefined variable %s" (Show.show<Id.v> v))
    | UndefinedFunction(l) ->
      failwith (Printf.sprintf "Undefined function %s" (Show.show<Id.l> l))
    | NotFunction(exp, _) ->
      failwith (Printf.sprintf "Callee is not a function: %s" (Show.show<Syntax.exp> exp))
    | NotPrimitive(exp) ->
      failwith (Printf.sprintf "Primitive type expected, but not primitive: %s" (Show.show<Syntax.exp> exp))
    | NotArray(v) ->
      failwith (Printf.sprintf "Array expected, but not an array: %s" (Show.show<Id.v> v))
