open Definition
open Util

type statement =
  | Label       of Id.l
  | Assignments of FlatExp.assignment list
  | Sequence    of statement list
  | Block       of Id.v variable list * statement list
  | BranchEq    of Id.v * Id.v * Id.l
  | BranchLt    of Id.v * Id.v * Id.l
  | BranchZero  of Id.v * Id.l
  | Goto        of Id.l
  | Return      of Id.v
  | ReturnVoid
    deriving (Show)

type t =
  | Function of Id.v function_signature * statement
  | GlobalVariable of Id.v variable
  | Array of Id.v array_signature
    deriving (Show)

let assign_const const =
  let id = Id.gen () in
  (id, [{ FlatExp.set = id; FlatExp.exp = FlatExp.Const(const) }])

type statement_environment = { continue : Id.l option; break : Id.l option }
    deriving (Show)

let rec convert_statement env stat =
  Printf.printf "%s\n" (Show.show<statement_environment> env);
  let go = convert_statement env in
  let go_option = BatOption.map go in
  let expand_branch {FlatExp.result = id1; FlatExp.chain = ass1}
      {FlatExp.result = id2; FlatExp.chain = ass2}
      stat_true
      stat_false
      constructor =
    let true_label = Id.gen_label "beq_true" in
    let end_label = Id.gen_label "beq_end" in
    Sequence([Assignments(ass1 @ ass2);
              constructor id1 id2 true_label]
             @ option_to_list (go_option stat_false)
             @ [Goto end_label;
                Label true_label;
                go stat_true;
                Label end_label])
  in
  match stat with
    | FlatExp.Label (l) ->
      Label(l)
    | FlatExp.Assignments (assignments) ->
      Assignments(assignments)
    | FlatExp.Block (variables, stats) ->
      Block(variables, List.map go stats)
    | FlatExp.IfEq (exp1, exp2, stat_true, stat_false) ->
      expand_branch exp1 exp2 stat_true stat_false (fun id1 id2 label -> BranchEq(id1, id2, label))
    | FlatExp.IfLt (exp1, exp2, stat_true, stat_false) ->
      expand_branch exp1 exp2 stat_true stat_false (fun id1 id2 label -> BranchLt(id1, id2, label))
    | FlatExp.IfTrue ({FlatExp.result = flag; FlatExp.chain = ass}, stat_true, Some(stat_false)) ->
      let false_label = Id.gen_label "false" in
      let end_label = Id.gen_label "end" in
      Sequence([Assignments(ass);
                BranchZero(flag, false_label);
                go stat_true;
                Goto end_label;
                Label false_label;
                go stat_false;
                Label end_label])
    | FlatExp.IfTrue ({FlatExp.result = flag; FlatExp.chain = ass}, stat_true, None) ->
      let end_label = Id.gen_label "end" in
      Sequence([Assignments(ass);
                BranchZero(flag, end_label);
                go stat_true;
                Label end_label])
    | FlatExp.Goto(l) -> Goto(l)
    | FlatExp.Continue ->
      (match env.continue with
        | Some(l) -> Goto(l)
        | None -> failwith "Unexpected continue.  No current continue label.")
    | FlatExp.Break ->
      (match env.break with
        | Some(l) -> Goto(l)
        | None -> failwith "Unexpected break.  No current break label.")
    | FlatExp.Return {FlatExp.result = result; FlatExp.chain = ass} ->
      Sequence [Assignments ass; Return result]
    | FlatExp.ReturnVoid -> ReturnVoid
    | FlatExp.Nop -> Sequence []

let convert_top = function
  | FlatExp.Function (fun_sig, stat) ->
    Function(fun_sig, convert_statement {continue = None; break = None} stat)
  | FlatExp.GlobalVariable (var) ->
    GlobalVariable(var)
  | FlatExp.Array (array_sig) ->
    Array(array_sig)

let convert =
  List.map convert_top
