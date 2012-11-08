{
(* lexerが利用する変数、関数、型などの定義 *)
open Parser
open Type
}

(* 正規表現の略記 *)
let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

rule token = parse
| space+
    { token lexbuf }
| "(*"
    { comment lexbuf; (* ネストしたコメントのためのトリック *)
      token lexbuf }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| "true"
    { BOOL(true) }
| "false"
    { BOOL(false) }
| "not"
    { NOT }
| digit+ (* 整数を字句解析するルール *)
    { let i = int_of_string (Lexing.lexeme lexbuf) in
      let rec f x = if x = 0 then (0, 0)
                    else let (y, z) = f (x lsr 1) in
                         (x land 1 + y, z + 1) in
      let (a, b) = f i in
      if i > 0 && a = 1 && b > 0 then BIN(b-1) else INT(i) }
| digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)?
    { FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
| '-' (* -.より後回しにしなくても良い? 最長一致? *)
    { MINUS }
| '+' (* +.より後回しにしなくても良い? 最長一致? *)
    { PLUS }
| "-."
    { MINUS_DOT }
| "+."
    { PLUS_DOT }
| "*"
    { AST }
| "/"
    { SLASH }
| "*."
    { AST_DOT }
| "/."
    { SLASH_DOT }
| "=="
    { DOUBLE_EQUAL }
| ":="
    { COLON_EQUAL }
| '='
    { EQUAL }
| "<>"
    { LESS_GREATER }
| "<="
    { LESS_EQUAL }
| ">="
    { GREATER_EQUAL }
| "->"
    { ARROW }
| "|"
    { PIPE }
| '<'
    { LESS }
| '>'
    { GREATER }
| "&&"
    { AND }
| "if"
    { IF }
| "then"
    { THEN }
| "else"
    { ELSE }
| "let"
    { LET }
| "in"
    { IN }
| "rec"
    { REC }
| "match"
    { MATCH }
| "with"
    { WITH }
| "ref"
    { REF }
| ','
    { COMMA }
| '_'
    { IDENT(Id.gentmp Type.Unit) }
| "[]"
    { EMPTY_BRACKET }
| "::"
    { DOUBLE_COLON }
| "Array.create" (* [XX] ad hoc *)
    { ARRAY_CREATE }
| "Array.make" (* [XX] ad hoc *)
    { ARRAY_CREATE }
| '.'
    { DOT }
| "<-"
    { LESS_MINUS }
| ';'
    { SEMICOLON }
| '!'
    { BANG }
| eof
    { EOF }
| lower (digit|lower|upper|'_')* (* 他の「予約語」より後でないといけない *)
    { IDENT(Lexing.lexeme lexbuf) }
| _
    { failwith
	(Printf.sprintf "unknown token %s near characters %d-%d"
	   (Lexing.lexeme lexbuf)
	   (Lexing.lexeme_start lexbuf)
	   (Lexing.lexeme_end lexbuf)) }
and comment = parse
| "*)"
    { () }
| "(*"
    { comment lexbuf;
      comment lexbuf }
| eof
    { Format.eprintf "warning: unterminated comment@." }
| _
    { comment lexbuf }
