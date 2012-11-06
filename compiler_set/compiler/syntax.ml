(* MinCamlの構文を表現するデータ型 *)
type t = 
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | Not of t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Sll of t * int
  | Sra of t * int
  | FNeg of t
  | FAdd of t * t
  | FSub of t * t
  | FMul of t * t
  | FDiv of t * t
  | Eq of t * t
  | LE of t * t
  | LT of t * t
  | If of t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of t * t list
  | Tuple of t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t
  | Match of t * (pattern * t) list
  | Nil
  | Cons of t * t
  | LetList of (list_matcher * Type.t option ref) * t * t
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }
and pattern = IntPattern of int | VarPattern of Id.t
and list_matcher = ListWithNil of Id.t list | ListWithoutNil of Id.t list



(***********************************************************************)
(* デバッグ用関数. tを出力. nは深さ. *)
let rec ind m = if m <= 0 then ()
                else (Printf.eprintf "  "; ind (m-1))
let rec string_of_matcher = function
  | ListWithNil(ids)    -> String.concat "::" (ids @ ["[]"])
  | ListWithoutNil(ids) -> String.concat "::" ids
let rec dbprint n t =
  ind n;
  match t with
  | Unit -> Printf.eprintf "Unit\n%!"
  | Bool true -> Printf.eprintf "Bool true\n%!"
  | Bool false -> Printf.eprintf "Bool false\n%!"
  | Int a -> Printf.eprintf "Int %d\n%!" a
  | Float a-> Printf.eprintf "Float %f\n%!" a
  | Not a -> Printf.eprintf "Not\n%!"; dbprint (n+1) a
  | Neg a -> Printf.eprintf "Neg\n%!"; dbprint (n+1) a
  | Add (a, b) -> Printf.eprintf "Add\n%!"; dbprint (n+1) a; dbprint (n+1) b
  | Sub (a, b) -> Printf.eprintf "Sub\n%!"; dbprint (n+1) a; dbprint (n+1) b
  | Mul (a, b) -> Printf.eprintf "Mul\n%!"; dbprint (n+1) a; dbprint (n+1) b
  | Sll (a, b) -> Printf.eprintf "Sll\n%!"; dbprint (n+1) a; dbprint (n+1) (Int b)
  | Sra (a, b) -> Printf.eprintf "Slr\n%!"; dbprint (n+1) a; dbprint (n+1) (Int b)
  | FNeg a -> Printf.eprintf "FNeg\n%!"; dbprint (n+1) a;
  | FAdd (a, b) -> Printf.eprintf "FAdd\n%!"; dbprint (n+1) a; dbprint (n+1) b
  | FSub (a, b) -> Printf.eprintf "FSub\n%!"; dbprint (n+1) a; dbprint (n+1) b
  | FMul (a, b) -> Printf.eprintf "FMul\n%!"; dbprint (n+1) a; dbprint (n+1) b
  | FDiv (a, b) -> Printf.eprintf "FDiv\n%!"; dbprint (n+1) a; dbprint (n+1) b
  | Eq (a, b) -> Printf.eprintf "Eq\n%!"; dbprint (n+1) a; dbprint (n+1) b
  | LE (a, b) -> Printf.eprintf "LE\n%!"; dbprint (n+1) a; dbprint (n+1) b
  | LT (a, b) -> Printf.eprintf "LT\n%!"; dbprint (n+1) a; dbprint (n+1) b
  | If (a, b, c) ->
      Printf.eprintf "If\n%!"; dbprint (n+1) a; ind n; Printf.eprintf "Then\n%!"; dbprint (n+1) b;
      ind n; Printf.eprintf "Else\n%!"; dbprint (n+1) c
  | Let ((a, t), b, c) ->
      Printf.eprintf "Let (%s:%s) =\n%!" a (Type.show t);
      dbprint (n+1) b; ind n; Printf.eprintf "In\n%!"; dbprint (n+1) c
  | Var a -> Printf.eprintf "Var %s\n%!" a
  | LetRec (f, a) -> Printf.eprintf "LetRec (%s:%s) %s =\n%!" (fst f.name) (Type.show (snd f.name)) (String.concat " " (List.map (fun (x,y) -> "(" ^ x ^ ":" ^ Type.show y) f.args) ^ ")") ; dbprint (n+1) f.body;
      ind n; Printf.eprintf "In\n%!" ; dbprint (n+1) a
  | App (a, l) -> Printf.eprintf "App\n%!"; dbprint (n+1) a; ind n; Printf.eprintf "To\n%!"; List.iter (dbprint (n+1)) l
  | Tuple l -> Printf.eprintf "Tuple\n%!"; List.iter (dbprint (n+1)) l
  | LetTuple (l, a, b) ->
      Printf.eprintf "Let (%s) =\n%!" (String.concat "," (List.map (fun (x,y) -> "(" ^ x ^ ":" ^ Type.show y ^ ")") l));
      dbprint (n+1) a;
      ind n; Printf.eprintf "in\n%!"; dbprint (n+1) b
  | Array (a, b) -> Printf.eprintf "Array\n$!"; dbprint (n+1) a; dbprint (n+1) b
  | Get (a, b) -> Printf.eprintf "Get\n%!"; dbprint (n+1) a; dbprint (n+1) b
  | Put (a, b, c) ->
      Printf.eprintf "Put\n%!"; dbprint (n+1) a; dbprint (n+1) b; dbprint (n+1) c
  | Match (exp, patterns) ->
    Printf.eprintf "Match\n%!"; dbprint (n+1) exp; ind n; Printf.eprintf "with\n%!";
    List.iter (function
      | (IntPattern i, body) -> ind n; Printf.eprintf "| %d -> \n%!" i; dbprint (n+1) exp
      | (VarPattern v, body) -> ind n; Printf.eprintf "| %s -> \n%!" v; dbprint (n+1) exp) patterns
  | LetList ((matcher, typ), body, rest) ->
    Printf.eprintf "Let (%s :: %s) =\n%!" (string_of_matcher matcher) ((function Some(t) -> Type.show t | None -> "") !typ);
    dbprint (n+1) body;
    ind n; Printf.eprintf "in\n%!"; dbprint (n+1) rest
  | Nil -> Printf.eprintf "List []\n%!"
  | Cons(x, xs) -> Printf.eprintf "Cons\n%!"; dbprint (n+1) x; dbprint (n+1) xs

