{ open Parser }

rule token = parse
  [' ' '\t' '\r'] { token lexbuf } (*whitespace*)
(*punctuations*)
| ['\n'] { EOL }
| "*/" { token lexbuf }
| '+' { PLUS } (*operators*)
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| '%' { MOD }
| ">=" { GEQ }
| "<=" { LEQ }
| "==" { EQUAL }
| "!=" { NEQ }
| "**" { SQUARE }
| '>' { GREATER }
| '<' { LESS }
| '(' { LPAREN }
| ')' { RPAREN }
| '[' { LBRACK }
| ']' { RBRACK }
| ',' { COMMA }
| '#' { COMMENT }
| '.' { BIND }
| '=' { ASSIGN }
| "and" { AND }
| "or" { OR }
| "in" { IN }
| "not" { NOT }
| ';'  { SEMI }
| ':'  { COLON }
| "string" { STRING }
| "num" { FLOAT }
| "bool" { BOOL }
| "point" { POINT }
| "if" { IF } (*controlling sequence*)
| "else" { ELSE }
| "then" { THEN }
| "end" { END }
| "for" { FOR }
| "while" { WHILE }
| "break" { BREAK }
| "continue" { CONTINUE }
| "print" { PRINT }
| "none" { NONE }
| "list" { LIST }
| "fn" { FN }
| "return" { RETURN }
| "true" { TRUE }
| "false"  { FALSE }
| ['0'-'9']+ as lit { INT(int_of_string lit) }
| ['0'-'9']*'.'['0'-'9']+ as lit { FLO(float_of_string lit) }
| '"'[^'"']*'"' as lit { STR(lit) }
| ['A'-'Z' 'a'-'z']+['A'-'Z' 'a'-'z' '0'-'9']* as lit { ID(lit) }
| eof { EOF }
| _  {raise (Failure("illegal character"))}

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
