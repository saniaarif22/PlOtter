open Ast

(*  printing the parsed AST *)

let _ =
let lexbuf = Lexing.from_channel stdin in
let prg = Parser.program Scanner.token lexbuf in
let result = string_of_program ([], List.rev prg) in
print_endline result;; 