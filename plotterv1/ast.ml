type binop = Add|Sub|Mul|Div|Mod|Equal|Neq|Less|Leq|Greater|Geq|And|Or|Square
(*binary operators*)
type uop = Uminus|Not (*uniary operators*)


type typ = String|Num|Bool|Point (*variable types*) 

type bind = typ * string


type expr = (*expressions*)
    Literal of int
  | Binop of expr * binop * expr (*a + 2*)
  | Id of string (*identifiers*)
  | Num of float(*3.2*)
  | Bool of bool (*True*)
  | Point of float * float
  | String of string(*'abcdefg'*)
  | Uniop of uop * expr (*-2 or ~True*)
  | Assign of string*expr (*a=2*)
  | Call of  string * expr list(*function call*)
  | Noexpr

type stmt = (* Statements *)
  Block of stmt list
  | Expr of expr
  | Var_decl of bind (* (type, id) *)
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Func_dec of func_decl
  | Break
  | Continue
    and func_decl = {(*function declaration*)
      (* return_type : string; *)
      fname : string;
      formals : bind list;
      locals : bind list;
      body : stmt list;
    }

type program = stmt list


(* Pretty Print Stuff *)


let string_of_uop op = match op with  
    | Uminus -> "-"
    | Not -> "not"
    


let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
	Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!="
      | Mod -> "%" 
      | And -> "&&" | Or ->"||"
      | Square -> "**"
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=") ^ " " ^
      string_of_expr e2
  | Uniop(op, e) -> (string_of_uop op) ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Bool(x) -> "bool" ^ (string_of_bool x)
  | String(x) -> "string" ^ x
  | Num(x) -> "num" ^ (string_of_float x)
  | Point(x,y) -> "point (" ^ (string_of_float x) ^ "," ^ (string_of_float y) ^ ")"
  | Noexpr -> ""

let string_of_vdecl (tp,id) =   "num " ^ " " ^ id ^ ";\n" 



let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Var_decl(tp, id) ->  "num " ^ " " ^ id ^ ";\n"
  | Continue -> "continue";
  | Break -> "break";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Func_dec(f) ->  string_of_fdecl f and 
 
    string_of_fdecl fdecl =
      fdecl.fname ^ "(" ^ 
      String.concat ", " (List.map string_of_vdecl fdecl.formals) ^
      ")\n{\n" ^
      String.concat "" (List.map string_of_vdecl fdecl.locals) ^
      String.concat "" (List.map string_of_stmt fdecl.body) ^
      "}\n"


let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)