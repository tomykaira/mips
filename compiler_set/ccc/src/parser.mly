%{
(* from http://www.cs.man.ac.uk/~pjj/bnf/c_syntax.bnf *)
open Definition
open Syntax
%}

%token <int>   INT_VAL
%token <float> FLOAT_VAL
%token <char>  CHAR_VAL

/* alphabetical order */
%token AND
%token AND_AND
%token ASTERISK
%token BANG
%token BANG_EQUAL
%token COMMA
%token COLON
%token DECREMENT
%token EQUAL
%token EQUAL_EQUAL
%token GT
%token GT_EQUAL
%token INCREMENT
%token LT
%token LT_EQUAL
%token L_BRACE
%token L_BRACKET
%token L_PAREN
%token MINUS
%token MINUS_EQUAL
%token PERCENT
%token PLUS
%token PLUS_EQUAL
%token PIPE_PIPE
%token R_BRACE
%token R_BRACKET
%token R_PAREN
%token SEMICOLON
%token SLASH
%token TILDE

%token BREAK
%token CASE
%token CONTINUE
%token DEFAULT
%token ELSE
%token GOTO
%token IF
%token RETURN
%token SHARP_DEFINE
%token SIZEOF
%token STRUCT
%token SWITCH
%token WHILE

%token EOF

%token <Syntax.storage_class> STORAGE_CLASS
%token <Definition.type_class> TYPE_CLASS

%token <Id.t> ID

%type <Id.t Syntax.t list> translation_unit
%start translation_unit

%%

translation_unit:
| external_decl
    { [$1] }
| translation_unit external_decl
    { $1 @ [$2] }

external_decl:
| function_definition
    { $1 }
| variable_definition
    { GlobalVariable($1) }
| array_definition
    { $1 }
| macro_definition
    { DefineMacro($1) }

function_definition:
| type_class ID L_PAREN parameter_list R_PAREN compound_stat
    { Function(signature $2 $1 $4, $6) }
| type_class ID L_PAREN R_PAREN compound_stat
    { Function(signature $2 $1 [], $5) }
| type_class ID L_PAREN parameter_list R_PAREN SEMICOLON
    { FunctionDeclaration(signature $2 $1 $4) }
| type_class ID L_PAREN R_PAREN SEMICOLON
    { FunctionDeclaration(signature $2 $1 []) }

parameter_list:
| parameter
    { [$1] }
| parameter_list COMMA parameter
    { $1 @ [$3] }

parameter:
| type_class ID
    { Parameter($1, $2) }
| type_class ASTERISK ID
    { PointerParameter($1, $3) }

type_class:
| TYPE_CLASS
    { $1 }

array_definition:
| type_class ID L_BRACKET INT_VAL R_BRACKET SEMICOLON
    { Array({id = $2; content_type = $1; size = $4}) }

macro_definition:
| SHARP_DEFINE ID const
    { ConstMacro($2, $3) }
| SHARP_DEFINE ID L_PAREN id_list R_PAREN L_PAREN exp R_PAREN
    { ExpMacro($2, $4, $7) }

id_list:
| ID
    { [$1] }
| id_list COMMA ID
    { $1 @ [$3] }


variable_definition_list:
| variable_definition
    { [$1] }
| variable_definition_list variable_definition
    { $1 @ [$2] }

variable_definition:
| type_class ID EQUAL const SEMICOLON
    { Variable($2, $1, $4) }


stat:
| labeled_stat
    { $1 }
| exp_stat
    { $1 }
| compound_stat
    { $1 }
| selection_stat
    { $1 }
| iteration_stat
    { $1 }
| jump_stat
    { $1 }

labeled_stat:
| ID COLON stat
    { Label(Id.L $1, $3) }

exp_stat:
| exp SEMICOLON
    { Exp($1) }
/* |     SEMICOLON */

compound_stat:
| L_BRACE stat_list R_BRACE
    { Block([], $2) }
| L_BRACE variable_definition_list stat_list R_BRACE
    { Block($2, $3) }
/* | L_BRACE decl_list           R_BRACE */
/* | L_BRACE                     R_BRACE */

stat_list:
| stat
    { [$1] }
| stat_list stat
    { $1 @ [$2] }

selection_stat:
| IF L_PAREN exp R_PAREN stat ELSE stat
    { If($3, $5, Some($7)) }
| IF L_PAREN exp R_PAREN stat
    { If($3, $5, None) }
| SWITCH L_PAREN exp R_PAREN L_BRACE case_definitions R_BRACE
    { Switch($3, $6) }

case_definitions:
| case_definition
    { [$1] }
| case_definitions case_definition
    { $1 @ [$2] }

case_definition:
| CASE const COLON stat
    { SwitchCase($2, $4) }
| DEFAULT COLON stat
    { DefaultCase($3) }

iteration_stat:
| WHILE L_PAREN exp R_PAREN stat
    { While($3, $5) }
/* | 'do' stat 'while' L_PAREN exp R_PAREN SEMICOLON */
/* | 'for' L_PAREN exp SEMICOLON exp SEMICOLON exp R_PAREN stat */
/* | 'for' L_PAREN exp SEMICOLON exp SEMICOLON	R_PAREN stat */
/* | 'for' L_PAREN exp SEMICOLON	SEMICOLON exp R_PAREN stat */
/* | 'for' L_PAREN exp SEMICOLON	SEMICOLON	R_PAREN stat */
/* | 'for' L_PAREN	SEMICOLON exp SEMICOLON exp R_PAREN stat */
/* | 'for' L_PAREN	SEMICOLON exp SEMICOLON	R_PAREN stat */
/* | 'for' L_PAREN	SEMICOLON	SEMICOLON exp R_PAREN stat */
/* | 'for' L_PAREN	SEMICOLON	SEMICOLON	R_PAREN stat */

jump_stat:
| GOTO ID SEMICOLON
    { Goto(Id.L $2) }
| CONTINUE SEMICOLON
    { Continue }
| BREAK SEMICOLON
    { Break }
| RETURN exp SEMICOLON
    { Return(Some($2)) }
| RETURN     SEMICOLON
    { Return(None) }

exp:
| assignment_exp
    { $1 }

assignment_exp:
| conditional_exp
    { $1 }
| assignee_exp EQUAL assignment_exp
    { Assign($1, $3) }
| assignee_exp PLUS_EQUAL assignment_exp
    { Assign($1, Add(ref_of $1, $3)) }
| assignee_exp MINUS_EQUAL assignment_exp
    { Assign($1, Sub(ref_of $1, $3)) }

assignee_exp:
| ID
    { VarSet($1) }
| ID L_BRACKET exp R_BRACKET
    { ArraySet($1, $3) }

/* TODO */
/* assignment_operator: */
/* | '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' */
/* | '>>=' | '&=' | '^=' | '|=' */

conditional_exp:
| binary_exp
    { $1 }
/* | binary_exp '?' exp ':' conditional_exp */
/*     { ConditionalOperator($1, $3, $5) } */

binary_exp:
| equality_exp
    { $1 }
/* | and_exp AND equality_exp */
/*     { BitAnd($1, $3) } */
/* | exclusive_or_exp '^' and_exp */
/*     { BitXor($1, $3) } */
/* | inclusive_or_exp '|' exclusive_or_exp */
/*     { BitOr($1, $3) } */
| binary_exp AND_AND equality_exp
    { And($1, $3) }
| binary_exp PIPE_PIPE equality_exp
    { Or($1, $3) }

equality_exp:
| shift_expression
    { $1 }
| equality_exp EQUAL_EQUAL shift_expression
    { Equal($1, $3) }
| equality_exp BANG_EQUAL shift_expression
    { Not(Equal($1, $3)) }
| equality_exp LT shift_expression
    { LessThan($1, $3) }
| equality_exp GT shift_expression
    { GreaterThan($1, $3) }
| equality_exp LT_EQUAL shift_expression
    { Not(GreaterThan($1, $3)) }
| equality_exp GT_EQUAL shift_expression
    { Not(LessThan($1, $3)) }

shift_expression:
| additive_exp
    { $1 }
/* | shift_expression '<<' additive_exp */
/* | shift_expression '>>' additive_exp */

additive_exp:
|  mult_exp
    { $1 }
| additive_exp PLUS mult_exp
    { Add($1, $3) }
| additive_exp MINUS mult_exp
    { Sub($1, $3) }

mult_exp:
| cast_exp
    { $1 }
| mult_exp ASTERISK cast_exp
    { Mul($1, $3) }
| mult_exp SLASH cast_exp
    { Div($1, $3) }
| mult_exp PERCENT cast_exp
    { Mod($1, $3) }

cast_exp:
| unary_exp
    { $1 }
/* | L_PAREN type_name R_PAREN cast_exp */
/*     { TypeCast($2, $4) } */

unary_exp:
| postfix_exp
    { $1 }
| PLUS cast_exp
    { $2 }
| MINUS cast_exp
    { Negate($2) }
| BANG cast_exp
    { Not($2) }

postfix_exp:
| primary_exp
    { $1 }
| ID L_PAREN argument_exp_list R_PAREN
    { CallFunction(Id.L $1, $3) }
| ID L_PAREN R_PAREN
    { CallFunction(Id.L $1, []) }

primary_exp:
| ID
    { Var($1) }
| ID L_BRACKET exp R_BRACKET
    { ArrayRef($1, $3) }
| const
    { Const($1) }
| L_PAREN exp R_PAREN
    { $2 }

argument_exp_list:
| assignment_exp
    { [$1] }
| argument_exp_list COMMA assignment_exp
    { $1 @ [$3] }

const:
| INT_VAL
    { IntVal($1) }
| CHAR_VAL
    { CharVal($1) }
| FLOAT_VAL
    { FloatVal($1) }
