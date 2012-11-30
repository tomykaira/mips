type statement =
  | Label       of Id.l
  | Assignments of FlatExp.assignment list
  | Call        of Id.l * Id.v list      (* if Exp has function call *)
  | Sequence    of statement list
  | Block       of Syntax.variable list * statement list
  | BranchZero  of Id.v * Id.l
  | BranchEqual of Id.v * Id.v * Id.l
  | Goto        of Id.l
  | Return      of Id.v
  | ReturnVoid
    deriving (Show)

type t =
  | Function of Id.l * Syntax.type_class * Syntax.parameter list * statement
  | GlobalVariable of Syntax.variable
    deriving (Show)

let assign_const const =
  let id = Id.gen () in
  (id, [{ FlatExp.set = id; FlatExp.exp = FlatExp.Const(const) }])

type statement_environment = { continue : Id.l option; break : Id.l option }

let rec convert_statement env stat =
  let { continue = current_cont; break = current_break } = env in
  let go = convert_statement env in
  match stat with
    | FlatExp.Label (l, stat) ->
      Sequence([Label(l); go stat])
    | FlatExp.Call (assignments, f, args) ->
      Sequence([Assignments(assignments); Call(f, args)])
    | FlatExp.Block (variables, stats) ->
      Block(variables, List.map go stats)
    | FlatExp.If ({FlatExp.result = flag; FlatExp.chain = ass}, stat_true, Some(stat_false)) ->
      let false_label = Id.gen_label "false" in
      let end_label = Id.gen_label "end" in
      Sequence([Assignments(ass);
                BranchZero(flag, false_label);
                go stat_true;
                Goto end_label;
                Label false_label;
                go stat_false;
                Label end_label])
    | FlatExp.If ({FlatExp.result = flag; FlatExp.chain = ass}, stat_true, None) ->
      let end_label = Id.gen_label "end" in
      Sequence([Assignments(ass);
                BranchZero(flag, end_label);
                go stat_true;
                Label end_label])
    | FlatExp.Switch ({FlatExp.result = flag; FlatExp.chain = ass}, all_cases) ->
      let end_label = Id.gen_label "switch_end" in
      let switch_env = { continue = current_cont; break = Some(end_label) } in
      let add_branch (header, body) (FlatExp.SwitchCase (const, stat)) =
        let label         = Id.gen_label "case" in
        let (id, ass)     = assign_const const in
        let new_statement = convert_statement switch_env stat in
        (Assignments ass :: BranchEqual(flag, id, label) :: header,
         Label(label) :: new_statement :: body)
      in
      let (cases, defaults) = BatList.partition (function FlatExp.SwitchCase _ -> true | FlatExp.DefaultCase _ -> false) all_cases in
      let (header, body) = List.fold_left add_branch ([], []) cases in
      let default_sequence =
        match defaults with
          | [] -> []
          | FlatExp.DefaultCase seq :: _ ->
            [convert_statement switch_env seq; Goto(end_label)]
          | _ -> failwith "Unexpected default case"
      in
      Sequence(header @ default_sequence @ body @ [Label(end_label)])
    | FlatExp.While ({FlatExp.result = flag; FlatExp.chain = ass}, stat) ->
      let start_label = Id.gen_label "while_start" in
      let end_label = Id.gen_label "while_end" in
      let new_statement = convert_statement { continue = Some(start_label); break = Some(end_label) } stat in
      Sequence [Label(start_label);
                Assignments ass;
                new_statement;
                Goto(start_label);
                Label(end_label)]
    | FlatExp.Goto(l) -> Goto(l)
    | FlatExp.Continue ->
      (match current_cont with
        | Some(l) -> Goto(l)
        | None -> failwith "Unexpected continue.  No current continue label.")
    | FlatExp.Break ->
      (match current_break with
        | Some(l) -> Goto(l)
        | None -> failwith "Unexpected break.  No current break label.")
    | FlatExp.Return {FlatExp.result = result; FlatExp.chain = ass} ->
      Sequence [Assignments ass; Return result]
    | FlatExp.ReturnVoid -> ReturnVoid
    | FlatExp.Nop -> Sequence []

let convert_top = function
  | FlatExp.Function (id, typ, params, stat) ->
    Function(id, typ, params, convert_statement {continue = None; break = None} stat)
  | FlatExp.GlobalVariable (var) ->
    GlobalVariable(var)

let convert =
  List.map convert_top
