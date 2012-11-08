%{
(* parserが利用する変数、関数、型などの定義 *)
open Syntax
let addtyp x = (x, Type.gentyp ())

%}

/* 字句を表すデータ型の定義 */
%token <bool> BOOL
%token <int> INT
%token <int> BIN
%token <float> FLOAT
%token NOT
%token MINUS
%token PLUS
%token MINUS_DOT
%token PLUS_DOT
%token AST
%token SLASH
%token AST_DOT
%token SLASH_DOT
%token EQUAL
%token DOUBLE_EQUAL
%token LESS_GREATER
%token LESS_EQUAL
%token GREATER_EQUAL
%token LESS
%token GREATER
%token IF
%token THEN
%token ELSE
%token <Id.t> IDENT
%token LET
%token IN
%token REC
%token COMMA
%token ARRAY_CREATE
%token DOT
%token LESS_MINUS
%token SEMICOLON
%token LPAREN
%token RPAREN
%token EOF
%token MATCH
%token WITH
%token PIPE
%token ARROW
%token EMPTY_BRACKET
%token DOUBLE_COLON
%token BANG
%token COLON_EQUAL
%token REF

/* 優先順位とassociativityの定義（低い方から高い方へ) */
%right prec_let
%right prec_semicolon
%right DOUBLE_COLON
%right SEMICOLON
%right prec_if
%right LESS_MINUS
%nonassoc ARROW
%left PIPE
%left COMMA
%left EQUAL LESS_GREATER LESS GREATER LESS_EQUAL GREATER_EQUAL COLON_EQUAL
%left PLUS MINUS PLUS_DOT MINUS_DOT
%left AST SLASH AST_DOT SLASH_DOT
%right prec_unary_minus
%left prec_app
%right BANG
%left DOT

/* 開始記号の定義 */
%type <Syntax.t> exp
%start exp

%%

simple_exp: /* 括弧をつけなくても関数の引数になれる式 */
| LPAREN exp RPAREN
    { $2 }
| LPAREN RPAREN
    { Unit }
| BOOL
    { Bool($1) }
| INT
    { Int($1) }
| BIN
    { Int(1 lsl $1) }
| FLOAT
    { Float($1) }
| IDENT
    { Var($1) }
| simple_exp DOT LPAREN exp RPAREN
    { Get($1, $4) }
| BANG simple_exp
    { Get($2, Int(0)) }

exp: /* 一般の式 */
| simple_exp
    { $1 }
| NOT exp
    %prec prec_app
    { Not($2) }
| MINUS exp
    %prec prec_unary_minus
    { match $2 with
    | Float(f) -> Float(-.f) (* -1.23などは型エラーではないので別扱い *)
    | e -> Neg(e) }
| exp PLUS exp /* 足し算を構文解析するルール */
    { Add($1, $3) }
| exp MINUS exp
    { Sub($1, $3) }
| exp AST exp
    { Mul($1, $3) }
| exp SLASH BIN
    { Sra($1, $3) }
| exp EQUAL exp
    { Eq($1, $3) }
| exp LESS_GREATER exp
    { Not(Eq($1, $3)) }
| exp LESS exp
    { LT($1, $3) }
| exp GREATER exp
    { LT($3, $1) }
| exp LESS_EQUAL exp
    { LE($1, $3) }
| exp GREATER_EQUAL exp
    { LE($3, $1) }
| IF exp DOUBLE_EQUAL EMPTY_BRACKET THEN exp ELSE exp
    %prec prec_if
    { If(IsNil($2), $6, $8) }
| IF exp THEN exp ELSE exp
    %prec prec_if
    { If($2, $4, $6) }
| MINUS_DOT exp
    %prec prec_unary_minus
    { FNeg($2) }
| exp PLUS_DOT exp
    { FAdd($1, $3) }
| exp MINUS_DOT exp
    { FSub($1, $3) }
| exp AST_DOT exp
    { FMul($1, $3) }
| exp SLASH_DOT exp
    { FDiv($1, $3) }
| LET IDENT EQUAL exp IN exp
    %prec prec_let
    { Let(addtyp $2, $4, $6) }
| LET REC fundef IN exp
    %prec prec_let
    { match fst ($3).name with
      | "read_int" | "read_float" when !Global.bin -> $5
      | _ -> LetRec($3, $5) }
| exp actual_args
    %prec prec_app
    { match ($1, $2) with
      | (Var("create_array"), [x;y]) -> Array(x, y)
      | _ -> App($1, $2) }
| elems
    { Tuple($1) }
| LET LPAREN tuple_pattern RPAREN EQUAL exp IN exp
    { LetTuple($3, $6, $8) }
| simple_exp DOT LPAREN exp RPAREN LESS_MINUS exp
    { Put($1, $4, $7) }
| exp SEMICOLON exp
    { Let((Id.gentmp Type.Unit, Type.Unit), $1, $3) }
| exp SEMICOLON
    %prec prec_semicolon
        { Let((Id.gentmp Type.Unit, Type.Unit), $1, Unit) }
| ARRAY_CREATE simple_exp simple_exp
    %prec prec_app
    { Array($2, $3) }
| MATCH exp WITH cases
    { Match($2, List.rev $4) }
| EMPTY_BRACKET
    { Nil }
| LET list_pattern EQUAL exp IN exp
    { LetList(($2, ref None), $4, $6) }
| exp DOUBLE_COLON exp
    { Cons($1, $3) }
| exp COLON_EQUAL exp
    { Put($1, Int(0), $3) }
| REF exp
    { Array(Int(1), $2) }

fundef:
| IDENT formal_args EQUAL exp
    { { name = addtyp $1; args = $2; body = $4 } }

formal_args:
| IDENT formal_args
    { addtyp $1 :: $2 }
| IDENT
    { [addtyp $1] }

actual_args:
| actual_args simple_exp
    %prec prec_app
    { $1 @ [$2] }
| simple_exp
    %prec prec_app
    { [$1] }

elems:
| elems COMMA exp
    { $1 @ [$3] }
| exp COMMA exp
    { [$1; $3] }

tuple_pattern:
| tuple_pattern COMMA IDENT
    { $1 @ [addtyp $3] }
| IDENT COMMA IDENT
    { [addtyp $1; addtyp $3] }

list_pattern:
| mid_list_pattern
    { ListWithoutNil($1) }
| mid_list_pattern DOUBLE_COLON EMPTY_BRACKET
    { ListWithNil($1) }
| IDENT DOUBLE_COLON EMPTY_BRACKET
    { ListWithNil([$1]) }

mid_list_pattern:
| mid_list_pattern DOUBLE_COLON IDENT
    { $1 @ [$3] }
| IDENT DOUBLE_COLON IDENT
    { [$1; $3] }

pattern:
| INT
    { IntPattern($1) }
| BIN
    { IntPattern(1 lsl $1) }
| IDENT
    { VarPattern($1) }

cases:
| pattern ARROW exp            { [($1, $3)] }
| PIPE pattern ARROW exp       { [($2, $4)] }
| cases PIPE pattern ARROW exp { ($3, $5) :: $1 }
