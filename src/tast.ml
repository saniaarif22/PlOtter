(* operations *)
type  ops =
        | Add | Sub | Mul  | Div | Mod
        | Equal | Neq | Less | Leq | Greater | Geq
        | And | Or
        | Square

type bool =
        | True | False


(*expressions*)
type expr =
    Literal_Num of float
  | Literal_Str of string
  | Binop of expr * ops * expr          (* Binary Ops *)
  | Id of string                        (* identifiers *)
  | Bool of bool                        (* True *)


type stmt = (* Statements *)
    Expr of expr
  | Var_Decl of string * string         (* (type, id) *)
  | Assign of expr * expr             (* a = 2 *)
  | Print of expr                       (* print 5 *)
  | Return of expr


type program = stmt list
