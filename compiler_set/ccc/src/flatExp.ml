(* Flatten Exp in Syntax *)
open Util

type exp =
  | Var            of Id.v
  | Const          of Syntax.const_value
  | And            of Id.v * Id.v
  | Or             of Id.v * Id.v
  | Equal          of Id.v * Id.v
  | LessThan       of Id.v * Id.v
  | GreaterThan    of Id.v * Id.v
  | Add            of Id.v * Id.v
  | Sub            of Id.v * Id.v
  | Mul            of Id.v * Id.v
  | Div            of Id.v * Id.v
  | Mod            of Id.v * Id.v
  | Not            of Id.v
  | Negate         of Id.v
  | CallFunction   of Id.l * Id.v list
      deriving (Show)

type assignment = { set : Id.v; exp : exp }
    deriving (Show)

type assignment_chain = { result : Id.v; chain : assignment list }
    deriving (Show)

type statement =
  | Label  of Id.l * statement
  | Call   of assignment list * Id.l * Id.v list      (* if Exp has function call *)
  | Block  of Syntax.variable list * statement list
  | If     of assignment_chain * statement * statement option
  | Switch of assignment_chain * switch_case list
  | While  of assignment_chain * statement
  | Goto   of Id.l
  | Continue
  | Break
  | Return of assignment_chain
  | ReturnVoid
  | Nop
and switch_case =
  | SwitchCase  of Syntax.const_value * statement
  | DefaultCase of statement
    deriving (Show)

type t =
  | Function of Syntax.function_signature * statement
  | GlobalVariable of Syntax.variable
    deriving (Show)



let rec expand_exp exp =
  let add1 e constructor =
    let {result = id1; chain = c1} = expand_exp e in
    let new_id = Id.gen () in
    { result = new_id; chain = {set = new_id; exp = constructor id1} :: c1 }
  in
  let concat2 e1 e2 constructor =
    let {result = id1; chain = c1} = expand_exp e1 in
    let {result = id2; chain = c2} = expand_exp e2 in
    let id = Id.gen () in
    { result = id; chain = {set = id; exp = constructor (id1, id2)} :: c2 @ c1}
  in
  match exp with
  | Syntax.Var(id) -> { result = id; chain = [] }
  | Syntax.Const(const) ->
    let id = Id.gen () in
    { result = id; chain = [{set = id; exp = Const(const)}] }
  | Syntax.Assign(Syntax.Var(id), e2) ->
    let {result = id2; chain = c2} = expand_exp e2 in
    { result = id; chain = {set = id; exp = Var(id2)} :: c2}
  | Syntax.Assign(_, e2) ->
    failwith "Assign to non-var expression is not supported"

  | Syntax.And (e1, e2)         -> concat2 e1 e2 (fun (t1, t2) -> And (t1, t2))
  | Syntax.Or (e1, e2)          -> concat2 e1 e2 (fun (t1, t2) -> Or (t1, t2))
  | Syntax.Equal (e1, e2)       -> concat2 e1 e2 (fun (t1, t2) -> Equal (t1, t2))
  | Syntax.LessThan (e1, e2)    -> concat2 e1 e2 (fun (t1, t2) -> LessThan (t1, t2))
  | Syntax.GreaterThan (e1, e2) -> concat2 e1 e2 (fun (t1, t2) -> GreaterThan (t1, t2))
  | Syntax.Add (e1, e2)         -> concat2 e1 e2 (fun (t1, t2) -> Add (t1, t2))
  | Syntax.Sub (e1, e2)         -> concat2 e1 e2 (fun (t1, t2) -> Sub (t1, t2))
  | Syntax.Mul (e1, e2)         -> concat2 e1 e2 (fun (t1, t2) -> Mul (t1, t2))
  | Syntax.Div (e1, e2)         -> concat2 e1 e2 (fun (t1, t2) -> Div (t1, t2))
  | Syntax.Mod (e1, e2)         -> concat2 e1 e2 (fun (t1, t2) -> Mod (t1, t2))

  | Syntax.Not (e) -> add1 e (fun t -> Not t)
  | Syntax.Negate (e) -> add1 e (fun t -> Negate t)

  | Syntax.PostIncrement (Syntax.Var(id)) ->
    let new_id = Id.gen () in
    let one = Id.gen () in
    { result = new_id; chain = [{set = id; exp = Add(id, one)};
                                {set = new_id; exp = Var(id)};
                                {set = one; exp = Const(Syntax.IntVal 1)}] }
  | Syntax.PostIncrement _ ->
    failwith "PostIncrement to non-var expression is not supported"

  | Syntax.PostDecrement (Syntax.Var(id)) ->
    let new_id = Id.gen () in
    let one = Id.gen () in
    { result = new_id; chain = [{set = id; exp = Sub(id, one)};
                                {set = new_id; exp = Var(id)};
                                {set = one; exp = Const(Syntax.IntVal 1)}] }
  | Syntax.PostDecrement _ ->
    failwith "PostDecrement to non-var expression is not supported"

  | Syntax.CallFunction (l, args) ->
    let args_binds = List.map expand_exp args in
    let (ids, chains) = List.split (List.map (fun {result = i; chain = c} -> (i, c)) args_binds) in
    let new_id = Id.gen () in
    { result = new_id; chain = {set = new_id; exp = CallFunction(l, ids)} :: (List.concat (List.rev chains))}

let rev_expand_exp exp =
  let { result =  r; chain = c } = expand_exp exp in
  { result =  r; chain = List.rev c }

let rec convert_case = function
  | Syntax.SwitchCase(const, stat) ->
    SwitchCase(const, convert_statement stat)
  | Syntax.DefaultCase(stat) ->
    DefaultCase(convert_statement stat)

and convert_statement = function
  | Syntax.Continue -> Continue
  | Syntax.Break -> Break
  | Syntax.Return(None) -> ReturnVoid
  | Syntax.Goto(l) ->
    Goto(l)

  | Syntax.Label(l, stat) ->
    Label(l, convert_statement stat)
  | Syntax.Exp(exp) ->
    let {chain = chain; _} = rev_expand_exp exp in
    let headed = BatList.drop_while (function {exp = CallFunction(_); _} -> false | _ -> true) (List.rev chain) in
    (match headed with
      | [] -> Nop
      | {exp = CallFunction(f, args); _} :: rest ->
        Call(rest, f, args)
      | _ -> failwith "unexpected case.  Head should be CallFunction")
  | Syntax.Block (variables, stats) ->
    Block(variables, List.map convert_statement stats)
  | Syntax.If(exp, stat_true, Some(stat_false)) ->
    If(rev_expand_exp exp, convert_statement stat_true, Some(convert_statement stat_false))
  | Syntax.If(exp, stat_true, None) ->
    If(rev_expand_exp exp, convert_statement stat_true, None)
  | Syntax.Switch(exp, cases) ->
    Switch(rev_expand_exp exp, List.map convert_case cases)
  | Syntax.While(exp, stat) ->
    While(rev_expand_exp exp, convert_statement stat)
  | Syntax.Return(Some(exp)) ->
    Return (rev_expand_exp exp)

let convert_top = function
  | Syntax.Function (signature, stat) ->
    Some(Function(signature, convert_statement stat))
  | Syntax.FunctionDeclaration (_) ->
    None
  | Syntax.GlobalVariable (var) ->
    Some(GlobalVariable(var))

let convert ts =
  let result = concat_map (function Some(x) -> [x] | None -> []) (List.map convert_top ts) in
  List.iter (print_endline $ Show.show<t>) result;
  result
