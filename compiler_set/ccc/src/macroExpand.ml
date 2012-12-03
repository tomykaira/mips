open Definition
open Util

type t =
  | Function of function_signature * Syntax.statement
  | FunctionDeclaration of function_signature
  | GlobalVariable of variable
  | Array of array_signature
    deriving (Show)

module NameMap = ExtendedMap.Make(Id.TStruct)

let add_list xys env = List.fold_left (fun env (x, y) -> NameMap.add x y env) env xys
let map_of_list l = add_list l NameMap.empty

type macro_set = {const : const_value NameMap.t; exp : (Syntax.exp list -> Syntax.exp) NameMap.t }

let empty_macros =
  { const = NameMap.empty; exp = NameMap.empty }

let rec convert_exp ({const = const_macros; exp = exp_macros} as macros) exp =
  let go = convert_exp macros in
  let convert_assignee = function
    | Syntax.VarSet(v) ->
      Syntax.VarSet(v)
    | Syntax.ArraySet(v, exp) ->
      Syntax.ArraySet(v, go exp)
  in
  match exp with
    | Syntax.Var(name) when NameMap.mem (Id.raw name) const_macros ->
      Syntax.Const(NameMap.find (Id.raw name) const_macros)
    | Syntax.Var(v) ->
      Syntax.Var(v)

    | Syntax.CallFunction(Id.L name, args) when NameMap.mem name exp_macros ->
      go ((NameMap.find name exp_macros) args)
    | Syntax.CallFunction(l, args) ->
      Syntax.CallFunction(l, List.map go args)

    | Syntax.ArrayRef(a, e)      -> Syntax.ArrayRef(a, go e)
    | Syntax.Const(v)            -> Syntax.Const(v)
    | Syntax.Assign(a, e)        -> Syntax.Assign(convert_assignee a, go e)
    | Syntax.And(e1, e2)         -> Syntax.And(go e1, go e2)
    | Syntax.Or(e1, e2)          -> Syntax.Or(go e1, go e2)
    | Syntax.Equal(e1, e2)       -> Syntax.Equal(go e1, go e2)
    | Syntax.LessThan(e1, e2)    -> Syntax.LessThan(go e1, go e2)
    | Syntax.GreaterThan(e1, e2) -> Syntax.GreaterThan(go e1, go e2)
    | Syntax.Add(e1, e2)         -> Syntax.Add(go e1, go e2)
    | Syntax.Sub(e1, e2)         -> Syntax.Sub(go e1, go e2)
    | Syntax.Mul(e1, e2)         -> Syntax.Mul(go e1, go e2)
    | Syntax.Div(e1, e2)         -> Syntax.Div(go e1, go e2)
    | Syntax.Mod(e1, e2)         -> Syntax.Mod(go e1, go e2)

    | Syntax.Not(e1)    -> Syntax.Not(go e1)
    | Syntax.Negate(e1) -> Syntax.Negate(go e1)


let rec convert_statement macros stat =
  let go = convert_statement macros in
  let go_exp = convert_exp macros in
  match stat with
    | Syntax.Label(l, stat) -> Syntax.Label(l, go stat)
    | Syntax.Exp(e)         -> Syntax.Exp(go_exp e)
    | Syntax.Block(variables, statements) ->
      Syntax.Block(variables, List.map go statements)
    | Syntax.If(e, stat_true, Some(stat_false)) ->
      Syntax.If(go_exp e, go stat_true, Some(go stat_false))
    | Syntax.If(e, stat_true, None) ->
      Syntax.If(go_exp e, go stat_true, None)
    | Syntax.Switch(e, cases) ->
      let new_cases =
        List.map
          (function Syntax.SwitchCase(const, stat) -> Syntax.SwitchCase(const, go stat)
            | Syntax.DefaultCase(stat) -> Syntax.DefaultCase(go stat)) cases
      in
      Syntax.Switch(go_exp e, new_cases)
    | Syntax.While(e, stat) ->
      Syntax.While(go_exp e, go stat)
    | Syntax.Return(Some(e)) ->
      Syntax.Return(Some(go_exp e))
    | stat -> stat

let rec replace_exp mapping exp =
  let go = replace_exp mapping in
  let var_of name =
    match NameMap.find name mapping with
      | Syntax.Var(new_v) -> new_v
      | _ -> failwith (Printf.sprintf "Variable name expected for %s, but an other expression" name)
  in
  let convert_assignee = function
    | Syntax.VarSet(v) when NameMap.mem (Id.raw v) mapping ->
      Syntax.VarSet(var_of (Id.raw v))
    | Syntax.VarSet(v) ->
      Syntax.VarSet(v)
    | Syntax.ArraySet(v, exp) when NameMap.mem (Id.raw v) mapping ->
      Syntax.ArraySet(var_of (Id.raw v), go exp)
    | Syntax.ArraySet(v, exp) ->
      Syntax.ArraySet(v, exp)
  in
  match exp with
    | Syntax.Var(name) when NameMap.mem (Id.raw name) mapping ->
      NameMap.find (Id.raw name) mapping
    | Syntax.Var(v) ->
      Syntax.Var(v)

    | Syntax.CallFunction(Id.L name, args) when NameMap.mem name mapping ->
      Syntax.CallFunction(Id.L (Id.raw (var_of name)), List.map go args)
    | Syntax.CallFunction(l, args) ->
      Syntax.CallFunction(l, List.map go args)

    | Syntax.ArrayRef(a, e)      -> Syntax.ArrayRef(a, go e)
    | Syntax.Const(v)            -> Syntax.Const(v)
    | Syntax.Assign(a, e)        -> Syntax.Assign(convert_assignee a, go e)
    | Syntax.And(e1, e2)         -> Syntax.And(go e1, go e2)
    | Syntax.Or(e1, e2)          -> Syntax.Or(go e1, go e2)
    | Syntax.Equal(e1, e2)       -> Syntax.Equal(go e1, go e2)
    | Syntax.LessThan(e1, e2)    -> Syntax.LessThan(go e1, go e2)
    | Syntax.GreaterThan(e1, e2) -> Syntax.GreaterThan(go e1, go e2)
    | Syntax.Add(e1, e2)         -> Syntax.Add(go e1, go e2)
    | Syntax.Sub(e1, e2)         -> Syntax.Sub(go e1, go e2)
    | Syntax.Mul(e1, e2)         -> Syntax.Mul(go e1, go e2)
    | Syntax.Div(e1, e2)         -> Syntax.Div(go e1, go e2)
    | Syntax.Mod(e1, e2)         -> Syntax.Mod(go e1, go e2)

    | Syntax.Not(e1)    -> Syntax.Not(go e1)
    | Syntax.Negate(e1) -> Syntax.Negate(go e1)

let convert ts =
  let add_macro t ({const = const; exp = exp} as macros) =
    match t with
      | Syntax.DefineMacro(Syntax.ConstMacro(id, value)) ->
        { macros with const = NameMap.add id value const}
      | Syntax.DefineMacro(Syntax.ExpMacro(id, params, value)) ->
        let mapper args =
          let mapping = zip params args in
          replace_exp (map_of_list mapping) value
        in
        { macros with exp = NameMap.add id mapper exp }
      | _ -> macros
  in
  let convert_t macros t =
    match t with
      | Syntax.Function (signature , stat) ->
        [Function(signature, convert_statement macros stat)]
      | Syntax.FunctionDeclaration(signature) ->
        [FunctionDeclaration(signature)]
      | Syntax.GlobalVariable(variable) ->
        [GlobalVariable(variable)]
      | Syntax.Array(arr) ->
        [Array(arr)]
      | Syntax.DefineMacro(_) -> []
  in
  let macros = List.fold_right add_macro ts empty_macros in
  concat_map (convert_t macros) ts
