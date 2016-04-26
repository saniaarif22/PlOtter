open Ast
open Semcheck

let convert (stmt_list) =
  check stmt_list;
  let rec create_expr = function
      | Ast.Literal_Num(l) -> (string_of_float l) ^ "0"
      | Ast.Literal_Str(l) -> l 
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
                  "num" -> "float" ^ " " ^ id ^ ";\n"
                | "string" -> "string" ^ " " ^ id ^ ";\n"
                | "point" -> "float" ^ " " ^ id ^ "[2];\n"
                | _ -> "bool" ^ " " ^ id ^ ";\n"
            ) 
   	   | Ast.Passign(v, e1, e2) -> 
            (* Setting the point elements seperately *)
            create_expr v ^ "[0] = " ^ ( create_expr e1 ) ^ ";\n" ^ 
            create_expr v ^ "[1] = " ^ ( create_expr e2 ) ^ ";\n"
   	   | Ast.Assign(v, e) -> create_expr v ^ " = " ^ ( create_expr e ) ^ ";\n"
   	   | Ast.Print(e) -> "put_in_svg( " ^ create_expr e ^ ");\n"
   	   | Ast.Return(expr) -> "return " ^ create_expr expr ^ ";\n"

   in
   
    "#include <iostream>\n#include <fstream>\n" ^
    "using namespace std;\n"^

    "ofstream f;\n"^
    "// SVG content\n"^
    "void put_in_svg(std::string content)\n"^
    "{\n"^
    "  f << \"<text x='250' y='150'>\\n\";\n"^
    "  f << content;\n"^
    "  f << \"\\n</text>\\n\";\n"^
    "}\n"^

    "void put_in_svg(float content)\n"^
    "{\n"^
    "  f << \"<text x='250' y='150'>\\n\";\n"^
    "  f << content;\n"^
    "  f << \"\\n</text>\\n\";\n"^
    "}\n"^

    "// Read input and generate SVG image\n"^

    "int main() {\n"^
    (* change the name to be the filename.svg based on the file which is ran *)
    "  f.open (\"hello.svg\");\n"^

    "  // Prolog for the SVG image\n"^
    "  f << \"<svg xmlns=\\\"http://www.w3.org/2000/svg\\\" xmlns:xlink=\\\"http://www.w3.org/1999/xlink\\\" width=\\\"1024\\\" height=\\\"768\\\">\"; \n"^
    "  f << \"\\n\"; \n"^

   String.concat "" (List.map create_stmt stmt_list) ^

    "  f << \"</svg>\"; \n" ^
   "return 0;\n}\n"
