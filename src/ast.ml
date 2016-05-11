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
  | Point of expr * expr
  | Literal_List of expr list               (* Eg [ expr, expr, .. ] *)
  | Binop of expr * ops * expr              (* Binary Ops *)
  | Id of string                            (* identifiers *)
  | Bool of bool                            (* True *)
  | Length of expr                          (* a.length() *)
  | Access of expr * expr                   (* a.at(3), a[3] *)
  


type stmt = (* Statements *)
    Expr of expr
  | Var_Decl of string * string             (* (type, id) *)
  | List_Decl of string * string
  | Passign of expr * expr * stmt                  (* (type, p1, p2) *)
  | Assign of expr * expr                   (* a = 2 *)
  | Append of expr * expr                   (* a.append(7) *)
  | Pop of expr                             (* a.pop() *)
  | Remove of expr * expr                   (* a.remove(3) *)
  | Fcall  of string * expr list            (* a.() *)
  | Print of expr                           (* print 5 *)
  | LineVar of expr * expr                  (* line(p,q) *)
  | LineRaw of expr * expr * expr * expr    (* line((3,4), (7,9)) *)
  | LinePX of expr * expr * expr            (* line((3,4), x) , line(x, (3,4)) *)
  | For of stmt * expr * stmt * stmt list   (* for i=0; i<5; i=i+1: *)
  | While  of expr * stmt list
  | Ifelse of expr * stmt list * stmt list
  | Return of expr
  | Noexpr
  | Fdecl of fdecl and
       fdecl = {
        fname : string;
        args  : stmt list;
        body  : stmt list;
      }
  


type program = {
                    funcs : stmt list;
                    main  : stmt list;
               }
                


(* Pretty Print *)

let rec string_of_expr = function
    Literal_Num(l) -> string_of_float l ^ "0"
  | Literal_Str(l) -> l
  | Point(e1, e2) -> "(" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ")"
  | Literal_List(l) -> "[" ^ (String.concat "," (List.map string_of_expr l)) ^ "]"
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
  | Length(v) -> string_of_expr v ^ ".length()\n"
  | Access(v, e) -> string_of_expr v ^ ".at(" ^ ( string_of_expr e ) ^ ")\n"
  
  

      
let rec string_of_stmt = function
    Expr(expr) -> string_of_expr expr ^ ""
  | Var_Decl(tp, id) -> tp ^ " " ^ id ^ "\n"
  | List_Decl(tp, id) -> "list " ^ tp ^ " " ^ id ^ "\n"
  | Passign(v, e1, e) -> " " ^ string_of_expr v ^ " = " ^ ( string_of_expr e1 ) ^ "\n"
  | Assign(v, e) -> "" ^ string_of_expr v ^ " = " ^ ( string_of_expr e )
  | Append(v, e) -> string_of_expr v ^ ".append(" ^ ( string_of_expr e ) ^ ")\n"
  | Pop(v) -> string_of_expr v ^ ".pop()\n"
  | Remove(v, e) -> string_of_expr v ^ ".remove(" ^ ( string_of_expr e ) ^ ")\n"
  | Fcall(v, el) ->  v ^ "("^ (String.concat "," (List.map string_of_expr el)) ^")\n"
  | Print(e) -> "print " ^ string_of_expr e ^ "\n"
  | LineVar(e1,e2)-> "line (" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ")" ^ "\n"
  | LineRaw(e1,e2,e3,e4)-> "line ( (" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ")" ^ "," ^ "(" ^ string_of_expr e3
                              ^ "," ^ string_of_expr e4 ^ ") )\n"
  | LinePX(e1, e2, e3)-> "line ( ( " ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ") ," ^ string_of_expr e3 ^ ") \n"
  | For(s1, e1, s2, body) -> "for " ^ string_of_stmt s1 ^ " ; " ^ string_of_expr e1 ^ " ; " ^ string_of_stmt s2 ^ ": \n"
                            ^ ( String.concat "\n\t" (List.map string_of_stmt body) )
                            ^ "\nend\n"
  | While(e, body) -> "while " ^ string_of_expr e ^ " :\n" ^ (String.concat "\n\t" (List.map string_of_stmt body)) ^ "\nend\n"
  | Ifelse(e, succ_stmt, else_stmt) -> "if " ^ string_of_expr e ^ " :\n" ^ (String.concat "\n\t" (List.map string_of_stmt succ_stmt)) ^ "\nelse:\n" ^ (String.concat "\n\t" (List.map string_of_stmt else_stmt)) ^ "end\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ "\n"
  | Noexpr -> ""
  | Fdecl(f) -> string_of_fdecl f and
  string_of_fdecl fdecl =
      "fn " ^ fdecl.fname ^ "(" ^ 
        ( String.concat ", " (List.map (fun s -> string_of_stmt s) fdecl.args) ) ^
         "):\n" ^
      ( String.concat "" (List.map string_of_stmt fdecl.body) ) ^
      "\nend\n"

let string_of_program prog =
  String.concat "\n" (List.map string_of_stmt prog.funcs)
  ^ "\n-----------\n" ^
  String.concat "\n" (List.map string_of_stmt prog.main)
  
