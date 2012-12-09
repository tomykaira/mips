{
  open Parser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let ascii = [' ' - '~']              (* 0 - 127 *)

  rule token = parse
    | space+
        { token lexbuf }
    | "/*"
        { comment lexbuf;
          token lexbuf }
    | "//" [^ '\n']*
        { token lexbuf } (* eat up one-line comments *)

    | "0x" (['0' - '9']|['a' - 'f']|['A' - 'F'])+
        { INT_VAL(int_of_string (Lexing.lexeme lexbuf)) }

    | digit+
        { INT_VAL(int_of_string (Lexing.lexeme lexbuf)) }
    | digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)?
        { FLOAT_VAL(float_of_string (Lexing.lexeme lexbuf)) }

    | '"'
        { let buffer = Buffer.create 20 in
          STRING (string buffer lexbuf) }

    | '&'
        { AND }
    | "&&"
        { AND_AND }
    | '*'
        { ASTERISK }
    | '!'
        { BANG }
    | ','
        { COMMA }
    | ':'
        { COLON }
    | "--"
        { DECREMENT }
    | "="
        { EQUAL }
    | "=="
        { EQUAL_EQUAL }
    | "!="
        { BANG_EQUAL }
    | "<="
        { LT_EQUAL }
    | ">="
        { GT_EQUAL }
    | ">"
        { GT }
    | ">>"
        { GT_GT }
    | "++"
        { INCREMENT }
    | "<"
        { LT }
    | "<<"
        { LT_LT }
    | '{'
        { L_BRACE }
    | '['
        { L_BRACKET }
    | '('
        { L_PAREN }
    | '-'
        { MINUS }
    | "-="
        { MINUS_EQUAL }
    | '%'
        { PERCENT }
    | '+'
        { PLUS }
    | "+="
        { PLUS_EQUAL }
    | "||"
        { PIPE_PIPE }
    | '}'
        { R_BRACE }
    | ']'
        { R_BRACKET }
    | ')'
        { R_PAREN }
    | ';'
        { SEMICOLON }
    | "/"
        { SLASH }
    | "~"
        { TILDE }

    | "break"
        { BREAK }
    | "case"
        { CASE }
    | "continue"
        { CONTINUE }
    | "default"
        { DEFAULT }
    | "else"
        { ELSE }
    | "goto"
        { GOTO }
    | "if"
        { IF }
    | "KEEP"
        { KEEP }
    | "return"
        { RETURN }
    | "struct"
        { STRUCT }
    | "#define"
        { SHARP_DEFINE }
    | "sizeof"
        { SIZEOF }
    | "switch"
        { SWITCH }
    | "while"
        { WHILE }

    | "auto"
        { STORAGE_CLASS(Syntax.Auto) }
    | "register"
        { STORAGE_CLASS(Syntax.Register) }
    | "static"
        { STORAGE_CLASS(Syntax.Static) }
    | "extern"
        { STORAGE_CLASS(Syntax.Extern) }
    | "typedef"
        { STORAGE_CLASS(Syntax.Typedef) }

    | "void"
        { TYPE_CLASS(Definition.Void) }
    | "char"
        { TYPE_CLASS(Definition.Char) }
    | "int"
        { TYPE_CLASS(Definition.Int) }
    | "long"
        { TYPE_CLASS(Definition.Long) }
    | "float"
        { TYPE_CLASS(Definition.Float) }
    | "signed"
        { TYPE_CLASS(Definition.Signed) }
    | "unsigned"
        { TYPE_CLASS(Definition.Unsigned) }

    | '\'' ascii '\''
        { CHAR_VAL((Lexing.lexeme lexbuf).[1]) }
    | "'\\n\'"
        { CHAR_VAL('\n') }
    | eof
        { EOF }
    | (digit|lower|upper|'_')+
        { ID(Lexing.lexeme lexbuf) }
    | _
        { failwith
            (Printf.sprintf "unknown token %s near characters %d-%d"
               (Lexing.lexeme lexbuf)
               (Lexing.lexeme_start lexbuf)
               (Lexing.lexeme_end lexbuf)) }
  and comment = parse
    | "*/"
        { () }
    | "/*"
        { comment lexbuf;
          comment lexbuf }
    | eof
        { failwith "warning: unterminated comment@." }
    | _
        { comment lexbuf }

  (* http://stackoverflow.com/questions/5793702/using-ocamllex-for-lexing-strings-the-tiger-compiler *)
  and  string buffer = parse
    | '"'
        { Buffer.contents buffer }
    | "\\t"
        { Buffer.add_char buffer '\t';
          string buffer lexbuf }
    | "\\n"
        { Buffer.add_char buffer '\n';
          string buffer lexbuf }
    | '\\' '"'
        { Buffer.add_char buffer '"';
          string buffer lexbuf }
    | '\\' '\\'
        { Buffer.add_char buffer '\\';
          string buffer lexbuf }
    | eof
        { raise End_of_file }
    | _ as char
        { Buffer.add_char buffer char;
          string buffer lexbuf }
