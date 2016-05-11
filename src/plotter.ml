
type action = Ast | Codegen (* | Tast | Cppsast *)

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast);
                ("-c", Codegen); (* ("-t", Tast) *) ]
  else Codegen in
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  match action with
    Ast -> print_string (Ast.string_of_program (program))
  | Codegen -> print_string (Codegen.convert (program))
  (*| Tast -> Cppsast.convert_to_cppast (List.rev program)*)
