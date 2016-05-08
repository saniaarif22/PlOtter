
open Ast

type t =  Num | Bool | String | Point | List
        | ListNum | ListString | ListPoint | ListBool

type texpr =
      Literal_Num of float * t
    | Literal_Str of string * t
    | Literal_List of texpr list * t
    | Binop of texpr * Ast.ops * texpr * t
    | Id of string * t
    | Bool of bool * t
    | Length  of texpr * t
  

type tstmt =
    Expr of texpr * t
  | Var_Decl of string * string * t
  | List_Decl of string * string *  t
  | Passign of texpr * texpr * texpr
  | Assign of texpr * texpr
  | Append of texpr * texpr
  | Remove of texpr * texpr
  | Access of texpr * texpr
  | Pop    of texpr
  | Fcall  of texpr * texpr list
  | Print of texpr
  | LineVar of texpr * texpr
  | LineRaw of texpr * texpr * texpr * texpr
  | For of tstmt * texpr * tstmt * tstmt list
  | While of texpr * tstmt list
  | Ifelse of texpr * tstmt list * tstmt list
  | Return of texpr
  | Noexpr
  | Fdecl of fdecl and
       fdecl = {
        fname : string;
        args  : tstmt list;
        body  : tstmt list;
      }

type program = tstmt list

(* Pretty Print Stuff *)

let typeof t =
  match t with
        | Num -> "num"
        | Bool -> "bool"
        | String -> "string"
        | Point -> "point"
        | ListNum    -> "listNum"
        | ListString -> "listString"
        | ListBool   -> "listBool"
        | ListPoint  -> "listPoint"
        | List -> "list"


let rec string_of_texpr = function
    Literal_Num(l, t) -> string_of_float l ^ typeof t
  | Literal_Str(l, t) -> l ^ typeof t
  | Literal_List(l, t) ->  typeof t
  | Id(s, t) -> s ^ typeof t
  | Length(v,_)  -> string_of_texpr v ^ ".length()"
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
  | Noexpr -> ""
  | Var_Decl(tp, id, t) -> tp ^ " " ^ id ^ "\n" ^ typeof t
  | List_Decl(tp, id, t) -> "list" ^ tp ^ " " ^ id ^ "\n" ^ typeof t
  | Passign(v, e1, e2) -> string_of_texpr v ^ " = (" ^ ( string_of_texpr e1 ) ^ "," ^ ( string_of_texpr e2 ) ^ ")\n"
  | Assign(v, e) -> string_of_texpr v ^ " = " ^ ( string_of_texpr e )
  | Append(v, e) -> string_of_texpr v ^ ".append(" ^ ( string_of_texpr e ) ^ ")"
  | Remove(v, e) -> string_of_texpr v ^ ".remove(" ^ ( string_of_texpr e ) ^ ")"
  | Access(v, e) -> string_of_texpr v ^ ".at(" ^ ( string_of_texpr e ) ^ ")"
  | Pop(v) -> string_of_texpr v ^ ".pop()"
  | Fcall(v, el)  -> string_of_texpr v ^ "("^ (String.concat "," (List.map string_of_texpr el)) ^")\n"
  | Print(e) -> "print " ^ string_of_texpr e ^ "\n"
  | LineVar(e1,e2)-> "line (" ^ string_of_texpr e1 ^ "," ^ string_of_texpr e2 ^ ")" ^ "\n"
  | LineRaw(e1,e2,e3,e4)-> "line ( (" ^ string_of_texpr e1 ^ "," ^ string_of_texpr e2 ^ ")" ^ "," ^ "("
                            ^ string_of_texpr e3 ^ "," ^ string_of_texpr e4 ^ ") )\n"
  | For(s1, e1, s2, body) -> "for " ^ string_of_tstmt s1 ^ " ; " ^ string_of_texpr e1 ^ " ; " ^ string_of_tstmt s2 ^ ": \n"
                            ^ ( String.concat "\n\t" (List.map string_of_tstmt body) )
                            ^ "\nend\n"
  | While(e, body) -> "while " ^ string_of_texpr e ^ " :\n" ^ (String.concat "\n\t" (List.map string_of_tstmt body)) ^ "\nend\n"
  | Ifelse(e, s1, s2) -> "if " ^ string_of_texpr e ^ " :\n" ^ (String.concat "\n\t" (List.map string_of_tstmt s1)) ^ "\nelse:\n" ^ (String.concat "\n\t" (List.map string_of_tstmt s2)) ^ "\nend\n"
  | Return(expr) -> "return " ^ string_of_texpr expr ^ "\n"
  | Fdecl(f) -> string_of_fdecl f and
  string_of_fdecl fdecl =
      "fn " ^ fdecl.fname ^ "(" ^ 
        ( String.concat ", " (List.map (fun s -> string_of_tstmt s ) fdecl.args) ) ^
         "):\n" ^
      ( String.concat "" (List.map string_of_tstmt fdecl.body) ) ^
      "\nend\n"
let string_of_tprogram stmts =
  String.concat "\n" (List.map string_of_tstmt stmts)
