open Definition
open Util

(* Limit exp to primitive instructions *)
type exp =
  | Mov            of Id.v
  | Const          of const_value
  | And            of Id.v * Id.v
  | Or             of Id.v * Id.v
  | Add            of Id.v * Id.v
  | Sub            of Id.v * Id.v
  | Negate         of Id.v
  | ArrayGet       of Id.v * Id.v
    deriving (Show)

type instruction =
  | Label       of Id.l
  | Assignment  of Id.v * exp
  | CallAndSet  of Id.v * Id.l * Id.v list      (* with variable binding *)
  | Call        of Id.l * Id.v list      (* just calling *)
  | Definition  of Id.v variable
  | BranchZero  of Id.v * Id.l
  | BranchEq    of Id.v * Id.v * Id.l
  | BranchLt    of Id.v * Id.v * Id.l
  | Goto        of Id.l
  | Return      of Id.v
  | ReturnVoid
  | ArraySet    of Id.v * Id.v * Id.v
    deriving (Show)

type t =
  | Function of Id.v function_signature * instruction list
  | GlobalVariable of Id.v variable
  | Array of Id.v array_signature
      deriving (Show)

(* Result of exp expansion *)
type expanded_exp = Exp of exp | Instructions of instruction list

let expand_exp assign_to exp =
  let local_branch branch_inst =
    let label_t = Id.gen_label "eq1" in
    let label_end = Id.gen_label "eq_end" in
    Instructions
      [branch_inst label_t;
       Assignment(assign_to, Const(IntVal(0)));
       Goto(label_end);
       Label(label_t);
       Assignment(assign_to, Const(IntVal(1)));
       Label(label_end)]
  in
  match exp with
  | FlatExp.Var(i)         -> Exp(Mov(i))
  | FlatExp.Const(c)       -> Exp(Const(c))
  | FlatExp.And(a, b)      -> Exp(And(a, b))
  | FlatExp.Or(a, b)       -> Exp(Or(a, b))
  | FlatExp.Add(a, b)      -> Exp(Add(a, b))
  | FlatExp.Sub(a, b)      -> Exp(Sub(a, b))
  | FlatExp.Negate(a)      -> Exp(Negate(a))
  | FlatExp.ArrayGet(a, b) -> Exp(ArrayGet(a, b))

  | FlatExp.Equal(a, b) ->
    local_branch (fun l -> BranchEq(a, b, l))
  | FlatExp.LessThan(a, b) ->
    local_branch (fun l -> BranchLt(a, b, l))
  | FlatExp.Not(a) ->
    local_branch (fun l -> BranchZero(a, l))

  | FlatExp.Mul(a, b) ->
    Instructions [CallAndSet(assign_to, Id.L "min_caml_mul", [a; b])]
  | FlatExp.Div(a, b) ->
    Instructions [CallAndSet(assign_to, Id.L "min_caml_div", [a; b])]
  | FlatExp.Mod(a, b) ->
    Instructions [CallAndSet(assign_to, Id.L "min_caml_mod", [a; b])]

  | FlatExp.CallFunction(l, args) ->
    Instructions [CallAndSet(assign_to, l, args)]

  | FlatExp.ArraySet(array, index, exp) ->
    Instructions [ArraySet(array, index, exp)]

let rec expand_statement = function
  | SimpleControl.Assignments(ass) ->
    let expand_ass {FlatExp.set = set; FlatExp.exp = exp} =
      match expand_exp set exp with
        | Exp(exp) ->
          [Assignment (set, exp)]
        | Instructions(insts) ->
          insts
    in
    concat_map expand_ass ass
  | SimpleControl.Sequence(stats) ->
    concat_map expand_statement stats
  | SimpleControl.Block(vars, stats) ->
    List.map (fun v -> Definition(v)) vars @ concat_map expand_statement stats

  | SimpleControl.Label(l)                 -> [Label(l)]
  | SimpleControl.BranchZero(id, l)        -> [BranchZero(id, l)]
  | SimpleControl.BranchEq(id1, id2, l)    -> [BranchEq(id1, id2, l)]
  | SimpleControl.BranchLt(id1, id2, l)    -> [BranchLt(id1, id2, l)]
  | SimpleControl.Goto(l)                  -> [Goto(l)]
  | SimpleControl.Return(x)                -> [Return(x)]
  | SimpleControl.ReturnVoid               -> [ReturnVoid]

(* To avoid empty label, insert void
   FIXME: This is not always type-safe. *)
let insert_return stats =
  stats @ [ReturnVoid]

let convert ts =
  let convert_fun = function
    | SimpleControl.Function(fun_sig, stat) ->
      Function(fun_sig, (insert_return $ expand_statement) stat)
    | SimpleControl.GlobalVariable(v) ->
      GlobalVariable v
    | SimpleControl.Array(a) ->
      Array a
  in
  let result = List.map convert_fun ts in
  List.iter (print_endline $ Show.show<t>) result;
  result
