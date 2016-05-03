
open Ast

type t = Num | Bool | String | Point

type texpr =
			Literal_Num of float * t
  			| Literal_Str of string * t
  			| Binop of texpr * Ast.ops * texpr * t
  			| Id of string * t
  			| Bool of bool * t

type tstmt =
    Expr of texpr * t
  | Var_Decl of string * string * t
  | List_Decl of string * string *  t
  | Passign of texpr * texpr * texpr
  | Assign of texpr * texpr
  | Print of texpr
  | LineVar of texpr * texpr
  | LineRaw of texpr * texpr * texpr * texpr
  | For of tstmt * texpr * tstmt * tstmt list
  | While of texpr * tstmt list
  | Return of texpr

type program = tstmt list

(* Pretty Print Stuff *)

let typeof t =
  match t with
        | Num -> "num"
        | Bool -> "bool"
        | String -> "string"
        | Point -> "point"


let rec string_of_texpr = function
    Literal_Num(l, t) -> string_of_float l ^ typeof t
  | Literal_Str(l, t) -> l ^ typeof t
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
  | List_Decl(tp, id, t) -> "list" ^ tp ^ " " ^ id ^ "\n" ^ typeof t
  | Passign(v, e1, e2) -> string_of_texpr v ^ " = (" ^ ( string_of_texpr e1 ) ^ "," ^ ( string_of_texpr e2 ) ^ ")\n"
  | Assign(v, e) -> string_of_texpr v ^ " = " ^ ( string_of_texpr e )
  | Print(e) -> "print " ^ string_of_texpr e ^ "\n"
  | LineVar(e1,e2)-> "line (" ^ string_of_texpr e1 ^ "," ^ string_of_texpr e2 ^ ")" ^ "\n"
  | LineRaw(e1,e2,e3,e4)-> "line ( (" ^ string_of_texpr e1 ^ "," ^ string_of_texpr e2 ^ ")" ^ "," ^ "("
                            ^ string_of_texpr e3 ^ "," ^ string_of_texpr e4 ^ ") )\n"
  | For(s1, e1, s2, body) -> "for " ^ string_of_tstmt s1 ^ " ; " ^ string_of_texpr e1 ^ " ; " ^ string_of_tstmt s2 ^ ": \n"
                            ^ ( String.concat "\n\t" (List.map string_of_tstmt body) )
                            ^ "\nend\n"
  | While(e, body) -> "while " ^ string_of_texpr e ^ " :\n" ^ (String.concat "\n\t" (List.map string_of_tstmt body)) ^ "\nend\n"
  | Return(expr) -> "return " ^ string_of_texpr expr ^ "\n"

let string_of_tprogram stmts =
  String.concat "\n" (List.map string_of_tstmt stmts)
