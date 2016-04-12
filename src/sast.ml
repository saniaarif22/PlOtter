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
