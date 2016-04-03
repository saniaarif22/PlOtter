open Ast
module StringMap = Map.Make(String)

let convert (contexts, finds, varmap) =
  let rec create_expr = function
      | Ast.Literal_Num(l) -> string_of_float l
      | Ast.Literal_Str(l) -> l
      | Ast.Id(s) -> s
      | Ast.Binop(e1, o, e2) -> 
      			string_of_expr e1 ^ " " ^
      			(match o with
		Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"
      		| Equal -> "==" | Neq -> "!="
      		| Mod -> "%" 
      		| And -> "&&" | Or ->"||"
      		| Square -> "**"
      		| Less -> "<" | Leq -> "<=" 
      		| Greater -> ">" | Geq -> ">="
      		) ^ " " ^ string_of_expr e2
      | Ast.Bool(x) -> if x = True then "true" else "false"
   
   in
   
   let rec create_stmt = function
   	   | Ast.Expr(expr) -> string_of_expr expr ^ "\n"
   	   | Ast.Var_Decl(tp, id) -> tp ^ " " ^ id ^ "\n"
   	   | Ast.Assign(v, e) -> v ^ " = " ^ ( string_of_expr e ) ^ "\n"
   	   | Ast.Print(e) -> "print " ^ string_of_expr e ^ "\n"
   	   | Ast.Return(expr) -> "return " ^ string_of_expr expr ^ "\n"

   in

   "#include <iostream>\n#include<filestream>\nusing namespace std;\nstream f;"