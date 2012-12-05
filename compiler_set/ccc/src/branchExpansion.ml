open Definition
open Util

type statement =
  | Label  of Id.l
  | Exp    of Id.v Syntax.exp
  | Block  of Id.v variable list * statement list
  | IfEq   of Id.v Syntax.exp * Id.v Syntax.exp * statement * statement option
  | IfLt   of Id.v Syntax.exp * Id.v Syntax.exp * statement * statement option
  | IfTrue of Id.v Syntax.exp * statement * statement option
  | Switch of Id.v Syntax.exp * switch_case list
  | Goto   of Id.l
  | Continue
  | Break
  | Return of Id.v Syntax.exp option
and switch_case =
  | SwitchCase  of const_value * statement
  | DefaultCase of statement
    deriving (Show)

type t =
  | Function of Id.v function_signature * statement
  | FunctionDeclaration of Id.v function_signature
  | GlobalVariable of Id.v variable
  | Array of Id.v array_signature
    deriving (Show)

let rec convert_case = function
  | Syntax.SwitchCase(const, stat) ->
    SwitchCase(const, convert_statement stat)
  | Syntax.DefaultCase(stat) ->
    DefaultCase(convert_statement stat)
and convert_statement exp =
  let go = convert_statement in
  let go_option = BatOption.map go in
  match exp with
    | Syntax.While(exp, stat) ->
      let start_label = Id.gen_label "while_start" in
      let expanded =
        [Label(start_label);
         go (Syntax.If(exp, Syntax.Block([], [stat; Syntax.Goto start_label]), None))]
      in
      Block([], expanded)

    | Syntax.If(Syntax.Equal(exp1, exp2), stat_true, stat_false) ->
      IfEq(exp1, exp2, go stat_true, go_option stat_false)
    | Syntax.If(Syntax.LessThan(exp1, exp2), stat_true, stat_false) ->
      IfLt(exp1, exp2, go stat_true, go_option stat_false)
    | Syntax.If(Syntax.GreaterThan(exp1, exp2), stat_true, stat_false) ->
      IfLt(exp2, exp1, go stat_true, go_option stat_false)

    (* && *)
    | Syntax.If(Syntax.And(exp1, exp2), stat_true, Some(stat_false)) ->
      let label = Id.gen_label "and_false" in
      go (Syntax.If(exp1,
                    Syntax.If(exp2, stat_true, Some(Syntax.Goto(label))),
                    Some(Syntax.Block([], [Syntax.Label(label); stat_false]))))
    | Syntax.If(Syntax.And(exp1, exp2), stat_true, None) ->
      go (Syntax.If(exp1, Syntax.If(exp2, stat_true, None), None))
    (* || *)
    | Syntax.If(Syntax.Or(exp1, exp2), stat_true, stat_false) ->
      let label = Id.gen_label "or_true" in
      go (Syntax.If(exp1,
                    Syntax.Block([], [Syntax.Label(label); stat_true]),
                    Some(Syntax.If(exp2, Syntax.Goto(label), stat_false))))

    (* ex: if (x) { ... } *)
    | Syntax.If(exp, stat_true, stat_false) ->
      IfTrue(exp, go stat_true, go_option stat_false)

    | Syntax.Label(l) ->
      Label(l)
    | Syntax.Continue -> Continue
    | Syntax.Break -> Break
    | Syntax.Return(Some(exp)) ->
      Return (Some(exp))
    | Syntax.Return(None) -> Return(None)
    | Syntax.Goto(l) ->
      Goto(l)
    | Syntax.Exp(exp) ->
      Exp(exp)
    | Syntax.Block (variables, stats) ->
      Block (variables, List.map go stats)
    | Syntax.Switch(exp, cases) ->
      Switch(exp, List.map convert_case cases)

let convert_top = function
  | MacroExpand.Function(signature, statement) ->
    Function(signature, convert_statement statement)
  | MacroExpand.FunctionDeclaration(d) -> FunctionDeclaration(d)
  | MacroExpand.GlobalVariable(v)      -> GlobalVariable(v)
  | MacroExpand.Array(a)               -> Array(a)

let convert ts =
  let result = List.map convert_top ts in
  print_endline (Show.show<t list> result);
  result
