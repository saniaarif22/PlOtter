(* operations *)
type  ops = 
        | Add | Sub | Mul  | Div | Mod 
        | Equal | Neq | Less | Leq | Greater | Geq 
        | And | Or 
        | Square

type bool =
        | True | False

type t = Num | Bool | String 

(*expressions*)
type expr = 
    Literal_Num of float                
  | Literal_Str of string
  | Binop of expr * ops * expr          (* Binary Ops *)
  | Id of string                        (* identifiers *)
  | Bool of bool                        (* True *)
  

type stmt = (* Statements *)
    Expr of expr
  | Var_Decl of string * string            (* (type, id) *)
  | Passign of expr * expr * expr          (* (type, p1, p2) *)
  | Assign of expr * expr                  (* a = 2 *)
  | Print of expr                          (* print 5 *)
  | LineVar of expr * expr                 (* line(p,q) *)
  | LineRaw of expr * expr * expr * expr   (* line((3,4), (7,9)) *)
  | Return of expr
  
  
type program = stmt list


(* Pretty Print Stuff *)

let rec string_of_expr = function
    Literal_Num(l) -> string_of_float l
  | Literal_Str(l) -> "\"" ^ l ^ "\""
  | Id(s) -> s
  | Binop(e1, o, e2) ->
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
  | Bool(x) -> if x = True then "true" else "false"


let rec string_of_stmt = function
    Expr(expr) -> string_of_expr expr ^ "\n"
  | Var_Decl(tp, id) -> tp ^ " " ^ id ^ "\n"
  | Passign(v, e1, e2) -> string_of_expr v ^ " = (" ^ ( string_of_expr e1 ) ^ "," ^ (string_of_expr e2) ^ ")\n"
  | Assign(v, e) -> string_of_expr v ^ " = " ^ ( string_of_expr e ) ^ "\n"
  | Print(e) -> "print " ^ string_of_expr e ^ "\n"
  | LineVar(e1,e2)-> "line (" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ")" ^ "\n" 
  | LineRaw(e1,e2,e3,e4)-> "line ( (" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ")" ^ "," ^ "(" ^ string_of_expr e3 ^ "," ^ string_of_expr e4 ^ ") )\n" 
  | Return(expr) -> "return " ^ string_of_expr expr ^ "\n"

let string_of_program stmts =
  String.concat "\n" (List.map string_of_stmt stmts)