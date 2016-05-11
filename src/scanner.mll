{ 
	open Err
	open Parser
	open Lexing
}

rule token = parse
  [' ' '\t' '\r'] { token lexbuf } (*whitespace*)
(*punctuations*)
| '\n' { EOL }
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
(*| "**" { SQUARE }*)
| '>' { GREATER }
| '<' { LESS }
| '(' { LPAREN }
| ')' { RPAREN }
| '[' { LBRACK }
| ']' { RBRACK }
| ',' { COMMA }
| '.' { OF }    
| '=' { ASSIGN }
| "and" { AND }
| "or" { OR }
(*| "in" { IN }*)
| "not" { NOT }
| ';'  { SEMI }
| ':'  { COLON }
| "string" { STRING }
| "num" { NUM }
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
| "line" { LINE }
| "none" { NONE }
| "list" { LIST }
| "hash" { HASH }

(* List related stuff *)
(* the '.' in front of append ensures its used as a function *)
| "append" { APPEND }
| "pop"    { POP }
| "remove" { REMOVE }
| "at"     { AT }
| "length" { LENGTH }

| "fn" { FN }
| "return" { RETURN }
| "true" { TRUE }
| "false"  { FALSE }
| '-'?(['0'-'9']+('.'['0'-'9']*)?) as lxm { LIT_NUM(float_of_string lxm) } (*Change to add negative*)
| ['"'][^'"']*['"'] as str { LIT_STR(str) }
| ['A'-'Z' 'a'-'z']+['A'-'Z' 'a'-'z' '0'-'9']* as i { ID(i) }
| eof { EOF }
| '#'   { singleLineComment lexbuf }
| "/*"  { multiLineComment lexbuf }
| _ as c { raise (Failure("Illegal character : " ^ Char.escaped c)) }

and singleLineComment = parse
| '\n'  {token lexbuf}
| _     {singleLineComment lexbuf}

and multiLineComment = parse
|"*/"   { token lexbuf }
| _     { multiLineComment lexbuf }