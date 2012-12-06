(* Flatten Exp in Syntax *)
open Definition
open Util

type exp =
  | Var            of Id.v
  | Const          of const_value
  | And            of Id.v * Id.v
  | Or             of Id.v * Id.v
  | Equal          of Id.v * Id.v
  | LessThan       of Id.v * Id.v
  | Add            of Id.v * Id.v
  | Sub            of Id.v * Id.v
  | Mul            of Id.v * Id.v
  | Div            of Id.v * Id.v
  | Mod            of Id.v * Id.v
  | Not            of Id.v
  | Negate         of Id.v
  | CallFunction   of Id.l * Id.v list
  | ArraySet       of Id.v * Id.v * Id.v
  | ArrayGet       of Id.v * Id.v
      deriving (Show)

type assignment = { set : Id.v; exp : exp }
    deriving (Show)

type assignment_chain = { result : Id.v; chain : assignment list }
    deriving (Show)

(* TODO: assignment_chains can go to Assignments *)
type statement =
  | Label       of Id.l
  | Assignments of assignment list
  | Block       of Id.v variable list * statement list
  | IfEq        of assignment_chain * assignment_chain * statement * statement option
  | IfLt        of assignment_chain * assignment_chain * statement * statement option
  | IfTrue       of assignment_chain * statement * statement option
  | Switch      of assignment_chain * switch_case list
  | While       of assignment_chain * statement
  | Goto        of Id.l
  | Continue
  | Break
  | Return      of assignment_chain
  | ReturnVoid
  | Nop
and switch_case =
  | SwitchCase  of const_value * statement
  | DefaultCase of statement
    deriving (Show)

type t =
  | Function of Id.v function_signature * statement
  | GlobalVariable of Id.v variable
  | Array of Id.v array_signature
    deriving (Show)



let rec expand_exp assign_to exp =
  let assign rest exp =
    match assign_to with
      | Some(assign_to) ->
        { result = assign_to; chain = {set = assign_to; exp = exp} :: rest }
      | None ->
        let id = Id.gen () in
        { result = id; chain = {set = id; exp = exp} :: rest }
  in
  let add1 e constructor =
    let {result = id1; chain = c1} = expand_exp None e in
    assign c1 (constructor id1)
  in
  let concat2 e1 e2 constructor =
    let {result = id1; chain = c1} = expand_exp None e1 in
    let {result = id2; chain = c2} = expand_exp None e2 in
    assign (c2 @ c1) (constructor (id1, id2))
  in
  match exp with
  | Syntax.Var(id) ->
    (match assign_to with
      | Some(assign_to) -> { result = assign_to; chain = [{ set = assign_to; exp = Var(id)}] }
      | None -> { result = id; chain = [] })
  | Syntax.Const(const) ->
    assign [] (Const(const))
  | Syntax.ArrayRef(id, index) ->
    add1 index (fun t -> ArrayGet(id, t))
  | Syntax.Assign(Syntax.VarSet(id), e2) ->
    expand_exp (Some id) e2
  | Syntax.Assign(Syntax.ArraySet(id, index), exp) ->
    let { result = index_id; chain = index_chain } = expand_exp None index in
    let { result = exp_id; chain = exp_chain } = expand_exp None exp in
    let dummy_id = Id.gen () in
    { result = exp_id;
      chain = { set = dummy_id; exp = ArraySet(id, index_id, exp_id) } :: index_chain @ exp_chain }

  | Syntax.And (e1, e2)         -> concat2 e1 e2 (fun (t1, t2) -> And (t1, t2))
  | Syntax.Or (e1, e2)          -> concat2 e1 e2 (fun (t1, t2) -> Or (t1, t2))
  | Syntax.Equal (e1, e2)       -> concat2 e1 e2 (fun (t1, t2) -> Equal (t1, t2))
  | Syntax.LessThan (e1, e2)    -> concat2 e1 e2 (fun (t1, t2) -> LessThan (t1, t2))
  | Syntax.Add (e1, e2)         -> concat2 e1 e2 (fun (t1, t2) -> Add (t1, t2))
  | Syntax.Sub (e1, e2)         -> concat2 e1 e2 (fun (t1, t2) -> Sub (t1, t2))
  | Syntax.Mul (e1, e2)         -> concat2 e1 e2 (fun (t1, t2) -> Mul (t1, t2))
  | Syntax.Div (e1, e2)         -> concat2 e1 e2 (fun (t1, t2) -> Div (t1, t2))
  | Syntax.Mod (e1, e2)         -> concat2 e1 e2 (fun (t1, t2) -> Mod (t1, t2))

  | Syntax.Not (e) -> add1 e (fun t -> Not t)
  | Syntax.Negate (e) -> add1 e (fun t -> Negate t)

  | Syntax.CallFunction (l, args) ->
    let args_binds = List.map (expand_exp None) args in
    let (ids, chains) = List.split (List.map (fun {result = i; chain = c} -> (i, c)) args_binds) in
    assign (List.concat (List.rev chains)) (CallFunction(l, ids))

let rev_expand_exp exp =
  let { result =  r; chain = c } = expand_exp None exp in
  { result =  r; chain = List.rev c }

let rec convert_case = function
  | BranchExpansion.SwitchCase(const, stat) ->
    SwitchCase(const, convert_statement stat)
  | BranchExpansion.DefaultCase(stat) ->
    DefaultCase(convert_statement stat)

and convert_statement stat =
  let go_option = function
    | Some(stat) -> Some(convert_statement stat)
    | None -> None
  in
  match stat with
  | BranchExpansion.Continue -> Continue
  | BranchExpansion.Break -> Break
  | BranchExpansion.Return(None) -> ReturnVoid
  | BranchExpansion.Goto(l) ->
    Goto(l)

  | BranchExpansion.Label(l) ->
    Label(l)
  | BranchExpansion.Exp(exp) ->
    let {chain = chain; _} = rev_expand_exp exp in
    Assignments chain
  | BranchExpansion.Block (variables, stats) ->
    Block(variables, List.map convert_statement stats)

  | BranchExpansion.IfEq(exp1, exp2, stat_true, stat_false) ->
    IfEq(rev_expand_exp exp1,
         rev_expand_exp exp2,
         convert_statement stat_true,
         go_option stat_false)
  | BranchExpansion.IfLt(exp1, exp2, stat_true, stat_false) ->
    IfLt(rev_expand_exp exp1,
         rev_expand_exp exp2,
         convert_statement stat_true,
         go_option stat_false)
  | BranchExpansion.IfTrue(exp, stat_true, stat_false) ->
    IfTrue(rev_expand_exp exp, convert_statement stat_true, go_option stat_false)

  | BranchExpansion.Switch(exp, cases) ->
    Switch(rev_expand_exp exp, List.map convert_case cases)
  | BranchExpansion.Return(Some(exp)) ->
    Return (rev_expand_exp exp)

let convert_top = function
  | BranchExpansion.Function (signature, stat) ->
    Some(Function(signature, convert_statement stat))
  | BranchExpansion.FunctionDeclaration (_) ->
    None
  | BranchExpansion.GlobalVariable (var) ->
    Some(GlobalVariable(var))
  | BranchExpansion.Array (signature) ->
    Some(Array(signature))

let convert (ts : BranchExpansion.t list) =
  let result = concat_map (function Some(x) -> [x] | None -> []) (List.map convert_top ts) in
  List.iter (print_endline $ Show.show<t>) result;
  result
