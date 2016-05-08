open Ast
open Semcheck

let convert prog =
  check prog.funcs;
  check prog.main;
  let rec create_expr = function
      | Ast.Literal_Num(l) -> (string_of_float l) ^ "0"
      | Ast.Literal_Str(l) -> l 
      | Ast.Literal_List(l)-> "{" ^ (String.concat "," (List.map  create_expr l) ) ^ "}"
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
   let remSemColon s = 
        Bytes.set s (String.length s - 1) ' '; s
   in
   let printFunArgs = function
       | Ast.Var_Decl(tp, id) -> 
            (match tp with
                  "num" -> "float" ^ " " ^ id
                | "string" -> "string" ^ " " ^ id
                | "point" -> "float" ^ " " ^ id ^ "[2]"
                | _ -> "bool" ^ " " ^ id
            ) 
       | Ast.List_Decl(tp, id) -> 
            (match tp with
                  "num" -> "vector <float>" ^ " *" ^ id
                | "string" -> "vector <string>" ^ " " ^ id
                | "point" -> "vector <array<float, 2>>" ^ " " ^ id ^ ";\n"
                | _ -> "vector <bool>" ^ " " ^ id ^ ";\n"
            )  
   in
   let rec create_stmt = function
   	   | Ast.Expr(expr) -> create_expr expr
   	   | Ast.Var_Decl(tp, id) -> 
            (match tp with
                  "num" -> "float" ^ " " ^ id ^ ";\n"
                | "string" -> "string" ^ " " ^ id ^ ";\n"
                | "point" -> "float" ^ " " ^ id ^ "[2];\n"
                | _ -> "bool" ^ " " ^ id ^ ";\n"
            ) 
       | Ast.List_Decl(tp, id) -> 
            (match tp with

                  "num" -> "vector <float>" ^ " " ^ id ^ ";\n"
                | "string" -> "vector <string>" ^ " " ^ id ^ ";\n"
                | "point" -> "vector <array<float, 2>>" ^ " " ^ id ^ ";\n"
                | _ -> "vector <bool>" ^ " " ^ id ^ ";\n"
            ) 
   	   | Ast.Passign(v, e1, e2) -> 
            (* Setting the point elements seperately *)
            create_expr v ^ "[0] = " ^ ( create_expr e1 ) ^ ";\n" ^ 
            create_expr v ^ "[1] = " ^ ( create_expr e2 ) ^ ";\n"
   	   | Ast.Assign(v, e) -> create_expr v ^ " = " ^ ( create_expr e ) ^ ";\n"
   	   | Ast.Append(v, e) -> create_expr v ^ ".push_back(" ^ ( create_expr e ) ^ ");\n"
   	   | Ast.Pop(v) -> create_expr v ^ ".pop_back();\n"
   	   | Ast.Remove(v,e) -> create_expr v ^ ".erase(" ^ (create_expr v) ^ ".begin() + " ^ ( create_expr e ) ^ ");\n"
   	   | Ast.Access(v,e) -> create_expr v ^ ".at(" ^ ( create_expr e ) ^ ");\n"
   	   | Ast.Length(v) -> create_expr v ^ ".size();\n"
   	   | Ast.Print(e) -> "put_in_svg( " ^ create_expr e ^ ");\n"
       | Ast.LineVar(e1, e2) -> "put_in_svg (" ^ create_expr e1 ^ "," ^ create_expr e2 ^");\n"
       | Ast.LineRaw(e1, e2, e3, e4) -> "put_in_svg (" ^ create_expr e1 ^ "," ^ create_expr e2 
                                      ^ "," ^ create_expr e3 ^ "," ^ create_expr e4 ^");\n"
       | Ast.For(s1, e1, s2, body) -> "for (" ^ create_stmt s1 ^ " " ^ create_expr e1 ^ " ; "
                                      ^ remSemColon (create_stmt s2 ) ^ " ) { \n" 
                                      ^ String.concat "" (List.map create_stmt body) ^ "\n } \n"
       | Ast.While(e, body) -> "while (" ^ create_expr e ^ ") { \n" ^ String.concat "" (List.map create_stmt body) ^ "}\n"
       | Ast.Ifelse(e, s1, s2) -> "if (" ^ create_expr e ^ ") { \n" ^ String.concat "" (List.map create_stmt s1)
            ^ "} else { \n" ^ String.concat "" (List.map create_stmt s2) ^ "}\n"
   	   | Ast.Return(expr) -> "return " ^ create_expr expr ^ ";\n"
   	   | Ast.Noexpr       -> ""
   	   | Ast.Fdecl(f)     -> string_of_fdecl f and
              string_of_fdecl fdecl =
                  "void " ^ fdecl.fname ^ "(" ^ 
                    ( String.concat ", " (List.map (fun s -> printFunArgs s) fdecl.args) ) ^
                     "){\n" ^
                  ( String.concat "" (List.map create_stmt fdecl.body) ) ^
                  "\n}\n"

   in
   
    "#include <iostream>\n#include <fstream>\n#include <vector>\n" ^
    "using namespace std;\n"^

    "ofstream f;\n"^
    "// SVG content\n"^
    
    "void put_in_svg(float p1[], float p2[])\n"^
    "{"^
    "  f << \"<line x1='\" + to_string(p1[0]) + \"' y1='\"+  to_string(p1[1])+\"' x2='\"+ to_string(p2[0]) +\"' y2='\"+ to_string(p2[1]) +\"' style='stroke:rgb(0,0,0);stroke-width:1'/>\\n\"; \n" ^
    "}\n"^
    
    "void put_in_svg(float x1, float y1, float x2, float y2)\n"^
    "{"^
    "  f << \"<line x1='\" + to_string(x1) + \"' y1='\"+  to_string(y1)+\"' x2='\"+ to_string(x2) +\"' y2='\"+ to_string(y2) +\"' style='stroke:rgb(0,0,0);stroke-width:1'/>\\n\"; \n" ^
    "}\n"^
    
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
    
    "//All user & library functions goes here\n" ^
    
    String.concat "" (List.map create_stmt prog.funcs) ^
    
    "//Main prog starts\n"^
    
    "int main() {\n"^
    (* change the name to be the filename.svg based on the file which is ran *)
    "  f.open (\"hello.svg\");\n"^

    "  // Prolog for the SVG image\n"^
    "  f << \"<svg xmlns=\\\"http://www.w3.org/2000/svg\\\" xmlns:xlink=\\\"http://www.w3.org/1999/xlink\\\" width=\\\"1024\\\" height=\\\"768\\\">\"; \n"^
    "  f << \"\\n\"; \n"^

   String.concat "" (List.map create_stmt prog.main) ^

    "  f << \"</svg>\"; \n" ^
   "return 0;\n}\n"
