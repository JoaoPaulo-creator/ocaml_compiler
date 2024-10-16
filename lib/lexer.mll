{
        open Parser
        exception LexError of error
}

let digit = ['0'-'9']
let integer = digit+
let whitespace = ['' '\t' '\n' '\r']
let letter = ['a'-'z' 'A'-'Z']
let identifier = letter (letter | digit | '_')*

rule token = parse
        | whitespace { token lexbuf }
        | "//" { comment lexbuf }
        | integer as i { INT(int_of_string i)}
        | "let" { LET }
        | "print" { PRINT }
        | '=' { EQUALS }
        | '+' { PLUS }
        | '-' { MINUS }
        | '*' { MULTIPLY }
        | '/' { DIVIDE }
        | '(' { LPAREN }
        | ')' { RPAREN }
        | ';' { SEMICOLON }
        | identifier as id { IDENTIFIER(id) }
        | eof { EOF }
        | _ { raise (LexError ("Unexpected character: " ^ Lexing.lexeme lexbuf))}

and comment = parse
        | '\n' { token lexbuf }
        | eof { EOF }
        | _ { comment lexbuf }
