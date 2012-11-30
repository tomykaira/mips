open Util

type instruction =
  | Label       of Id.l
  | Assignment  of Id.t * FlatExp.exp
  | Call        of Id.l * Id.t list      (* if Exp has function call *)
  | Definition  of Syntax.variable
  | BranchZero  of Id.t * Id.l
  | BranchEqual of Id.t * Id.t * Id.l
  | Goto        of Id.l
  | Return      of Id.t
  | ReturnVoid
    deriving (Show)

type t =
  | Function of Id.l * Syntax.type_class * Syntax.parameter list * instruction list
  | GlobalVariable of Syntax.variable
      deriving (Show)

let rec expand_statement = function
  | SimpleControl.Assignments(ass) ->
    List.map (fun {FlatExp.set = set; FlatExp.exp = exp} -> Assignment (set, exp)) ass
  | SimpleControl.Sequence(stats) ->
    concat_map expand_statement stats
  | SimpleControl.Block(vars, stats) ->
    List.map (fun v -> Definition(v)) vars @ concat_map expand_statement stats

  | SimpleControl.Label(l)                 -> [Label(l)]
  | SimpleControl.Call(l, id)              -> [Call(l, id)]
  | SimpleControl.BranchZero(id, l)        -> [BranchZero(id, l)]
  | SimpleControl.BranchEqual(id1, id2, l) -> [BranchEqual(id1, id2, l)]
  | SimpleControl.Goto(l)                  -> [Goto(l)]
  | SimpleControl.Return(x)                -> [Return(x)]
  | SimpleControl.ReturnVoid               -> [ReturnVoid]

(* To avoid empty label, insert void
   FIXME: This is not always type-safe. *)
let insert_return stats =
  stats @ [ReturnVoid]

let convert ts =
  let convert_fun = function
    | SimpleControl.Function(l, typ, params, stat) ->
      Function(l, typ, params, insert_return (expand_statement stat))
    | SimpleControl.GlobalVariable(v) ->
      GlobalVariable v
  in
  List.map convert_fun ts
