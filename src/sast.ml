open Ast

type texpr =
			Literal_Num of float * t               
  			| Literal_Str of string * t
  			| Binop of texpr * Ast.ops * texpr * t
  			| Id of string * t
  			| Bool of bool * t

type tstmt =
    Expr of texpr * t
  | Var_Decl of string * string * t         
  | Assign of texpr * texpr             
  | Print of texpr                     
  | Return of texpr

type program = tstmt list

(* Pretty Print Stuff *)

let typeof t = 
  match t with
        | Num -> "num"
        | Bool -> "bool"
        | String -> "string"

let rec string_of_texpr = function
    Literal_Num(l, t) -> string_of_float l ^ typeof t
  | Literal_Str(l, t) -> "\"" ^ l ^ "\"" ^ typeof t
  | Id(s, t) -> s ^ typeof t
  | Binop(e1, o, e2, t) ->
      string_of_texpr e1 ^ " " ^
      (match o with
  Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!="
      | Mod -> "%" 
      | And -> "&&" | Or ->"||"
      | Square -> "**"
      | Less -> "<" | Leq -> "<=" 
      | Greater -> ">" | Geq -> ">="
      ) ^ " " ^ string_of_texpr e2 ^ typeof t
  | Bool(x, t) -> if x = True then "true" else "false" ^ typeof t


let rec string_of_tstmt = function
    Expr(expr, t) -> string_of_texpr expr ^ "\n" ^ typeof t
  | Var_Decl(tp, id, t) -> tp ^ " " ^ id ^ "\n" ^ typeof t
  | Assign(v, e) -> string_of_texpr v ^ " = " ^ ( string_of_texpr e ) ^ "\n"
  | Print(e) -> "print " ^ string_of_texpr e ^ "\n"
  | Return(expr) -> "return " ^ string_of_texpr expr ^ "\n"

let string_of_program stmts =
  String.concat "\n" (List.map string_of_stmt stmts)
