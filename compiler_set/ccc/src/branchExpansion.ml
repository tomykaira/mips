open Definition

type statement =
  | Label  of Id.l
  | Exp    of Id.v Syntax.exp
  | Block  of (Id.v, Id.v Syntax.exp) variable list * statement list
  | IfEq   of Id.v Syntax.exp * Id.v Syntax.exp * statement * statement option
  | IfLt   of Id.v Syntax.exp * Id.v Syntax.exp * statement * statement option
  | IfTrue of Id.v Syntax.exp * statement * statement option
  | Goto   of Id.l
  | Return of Id.v Syntax.exp option
    deriving (Show)

type t =
  | Function of Id.v function_signature * statement
  | FunctionDeclaration of Id.v function_signature
  | GlobalVariable of Id.v global_variable
  | Array of Id.v array_signature
    deriving (Show)


type while_environment = { continue : Id.l option; break : Id.l option }
    deriving (Show)


let rec convert_statement env exp =
  let go = convert_statement env in
  let go_option = BatOption.map go in
  match exp with
    | Syntax.While(exp, stat) ->
      let start_label = Id.gen_label "while_start" in
      let break_label = Id.gen_label "while_end" in
      let expanded =
        [Label(start_label);
         convert_statement { continue = Some(start_label); break = Some(break_label) }
           (Syntax.If(exp, Syntax.Block([], [stat; Syntax.Goto start_label]), None));
         Label(break_label)]
      in
      Block([], expanded)

    | Syntax.If(Syntax.Not(exp), stat_true, Some(stat_false)) ->
      go (Syntax.If(exp, stat_false, Some(stat_true)))
    (* TODO: express NOP in better way *)
    | Syntax.If(Syntax.Not(exp), stat_true, None) ->
      go (Syntax.If(exp, Syntax.Block([], []), Some(stat_true)))

    | Syntax.If(Syntax.Equal(exp1, exp2), stat_true, stat_false) ->
      IfEq(exp1, exp2, go stat_true, go_option stat_false)
    | Syntax.If(Syntax.LessThan(exp1, exp2), stat_true, stat_false) ->
      IfLt(exp1, exp2, go stat_true, go_option stat_false)

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
    | Syntax.Continue ->
      (match env.continue with
        | Some(l) -> Goto(l)
        | None -> failwith "Unexpected continue.  No current continue label.")
    | Syntax.Break ->
      (match env.break with
        | Some(l) -> Goto(l)
        | None -> failwith "Unexpected break.  No current break label.")

    | Syntax.Label(l) ->
      Label(l)
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
      let end_label = Id.gen_label "switch_end" in
      let switch_env = { env with break = Some(end_label) } in
      let add_branch (cases, default) case =
        match case with
          | Syntax.SwitchCase (const, stats) ->
            (IfEq(exp, Syntax.Const(const), Block([], List.map (convert_statement switch_env) stats), None) :: cases,
             default)
          (* default case overrides existing value *)
          | Syntax.DefaultCase (stats) ->
            (cases, [Block([], List.map (convert_statement switch_env) stats)])
      in
      let (cases, default) = List.fold_left add_branch ([], []) cases in
      Block([], cases @ default @ [Label(end_label)])

let convert_top = function
  | MacroExpand.Function(signature, statement) ->
    Function(signature, convert_statement {continue = None; break = None} statement)
  | MacroExpand.FunctionDeclaration(d) -> FunctionDeclaration(d)
  | MacroExpand.GlobalVariable(v)      -> GlobalVariable(v)
  | MacroExpand.Array(a)               -> Array(a)

let convert ts =
  let result = List.map convert_top ts in
  print_endline (Show.show<t list> result);
  result
