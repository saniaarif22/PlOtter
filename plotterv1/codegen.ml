open Ast

let convert (stmt_list) =
  let rec create_expr = function
      | Ast.Literal_Num(l) -> string_of_float l
      | Ast.Literal_Str(l) -> "\"" ^ l ^ "\""
      | Ast.Id(s) -> s
      | Ast.Binop(e1, o, e2) -> 
      			create_expr e1 ^ " " ^
      			(match o with
		Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"
      		| Equal -> "==" | Neq -> "!="
      		| Mod -> "%" 
      		| And -> "&&" | Or ->"||"
      		| Square -> "**"
      		| Less -> "<" | Leq -> "<=" 
      		| Greater -> ">" | Geq -> ">="
      		) ^ " " ^ create_expr e2
      | Ast.Bool(x) -> if x = True then "true" else "false"
   
   in
   
   let rec create_stmt = function
   	   | Ast.Expr(expr) -> create_expr expr ^ ";\n"
   	   | Ast.Var_Decl(tp, id) -> 
            (match tp with
                  "num" -> "float"
                | "string" -> "string"
                | _ -> "bool"
            ) ^ " " ^ id ^ ";\n"
   	   | Ast.Assign(v, e) -> create_expr v ^ " = " ^ ( create_expr e ) ^ "\n"
   	   | Ast.Print(e) -> "put_in_svg( " ^ create_expr e ^ ");\n"
   	   | Ast.Return(expr) -> "return " ^ create_expr expr ^ ";\n"

   in
   
    "#include <iostream>\n#include <fstream>" ^
    "using namespace std;"^

    "ofstream f;"^
    "// SVG content"^
    "void put_in_svg(std::string content) "^
    "{"^
    "  f << \"<text x='250' y='150'>\n\";"^
    "  f << content;"^
    "  f << \"\\n</text>\n\";"^
    "}"^

    "// Read input and generate SVG image"^

    "int main() {"^
    (* change the name to be the filename.svg based on the file which is ran *)
    "  f.open (\"hello.svg\");"^

    "  // Prolog for the SVG image"^
    "  f << \"<svg xmlns=\\\"http://www.w3.org/2000/svg\\\" xmlns:xlink=\\\"http://www.w3.org/1999/xlink\\\"> width=\\\"1024\\\" height=\\\"768\\\"\"; " ^
    "  f << \"\\n\"; "^

   String.concat "" (List.map create_stmt stmt_list) ^
   "return 0;\n}\n"
