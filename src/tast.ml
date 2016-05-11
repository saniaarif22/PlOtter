(* operations *)
type  ops =
        | Add | Sub | Mul  | Div | Mod
        | Equal | Neq | Less | Leq | Greater | Geq
        | And | Or
        | Square

type bool =
        | True | False


(*texpressions*)
type texpr =
    Literal_Num of float
  | Literal_Str of string
  | Literal_List of texpr list           (* Eg [ texpr, texpr, .. ] *)
  | Binop of texpr * ops * texpr          (* Binary Ops *)
  | Id of string                        (* identifiers *)
  | Bool of bool                        (* True *)
  | Length of texpr                          (* a.length() *)


type tstmt = (* Statements *)
    Texpr of texpr
  | Var_Decl of string * string             (* (type, id) *)
  | List_Decl of string * string
  | Passign of texpr * texpr * texpr           (* (type, p1, p2) *)
  | Assign of texpr * texpr                   (* a = 2 *)
  | Append of texpr * texpr                   (* a.append(7) *)
  | Pop of texpr                             (* a.pop() *)
  | Remove of texpr * texpr                   (* a.remove(3) *)
  | Access of texpr * texpr                   (* a.at(3), a[3] *)
  | Fcall  of string * texpr list              (* a.() *)
  | Print of texpr                           (* print 5 *)
  | LineVar of texpr * texpr                  (* line(p,q) *)
  | LineRaw of texpr * texpr * texpr * texpr    (* line((3,4), (7,9)) *)
  | LinePX of texpr * texpr * texpr            (* line((3,4), x) , line(x, (3,4)) *)
  | For of tstmt * texpr * tstmt * tstmt list   (* for i=0; i<5; i=i+1: *)
  | While  of texpr * tstmt list
  | Ifelse of texpr * tstmt list * tstmt list
  | Return of texpr
  | Notexpr
  | Fdecl of fdecl and
       fdecl = {
        fname : string;
        args  : tstmt list;
        body  : tstmt list;
      }



type program = {
                    funcs : tstmt list;
                    main  : tstmt list;
               }



(* Pretty Print *)

let rec string_of_texpr = function
    Literal_Num(l) -> string_of_float l ^ "0"
  | Literal_Str(l) -> l
  | Literal_List(l) -> "[" ^ (String.concat "," (List.map string_of_texpr l)) ^ "]"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_texpr e1 ^ " " ^
      (match o with
	Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!="
      | Mod -> "%"
      | And -> "&&" | Or ->"||"
      | Square -> "**"
      | Less -> "<" | Leq -> "<="
      | Greater -> ">" | Geq -> ">="
      ) ^ " " ^ string_of_texpr e2
  | Bool(x) -> if x = True then "true" else "false"
  | Length(v) -> string_of_texpr v ^ ".length()\n"



let rec string_of_tstmt = function
    Texpr(texpr) -> string_of_texpr texpr ^ ""
  | Var_Decl(tp, id) -> tp ^ " " ^ id ^ "\n"
  | List_Decl(tp, id) -> "list " ^ tp ^ " " ^ id ^ "\n"
  | Passign(v, e1, e2) -> string_of_texpr v ^ " = (" ^ ( string_of_texpr e1 ) ^ "," ^ (string_of_texpr e2) ^ ")\n"
  | Assign(v, e) -> string_of_texpr v ^ " = " ^ ( string_of_texpr e )
  | Append(v, e) -> string_of_texpr v ^ ".append(" ^ ( string_of_texpr e ) ^ ")\n"
  | Pop(v) -> string_of_texpr v ^ ".pop()\n"
  | Remove(v, e) -> string_of_texpr v ^ ".remove(" ^ ( string_of_texpr e ) ^ ")\n"
  | Access(v, e) -> string_of_texpr v ^ ".at(" ^ ( string_of_texpr e ) ^ ")\n"
  | Fcall(v, el) ->  v ^ "("^ (String.concat "," (List.map string_of_texpr el)) ^")\n"
  | Print(e) -> "print " ^ string_of_texpr e ^ "\n"
  | LineVar(e1,e2)-> "line (" ^ string_of_texpr e1 ^ "," ^ string_of_texpr e2 ^ ")" ^ "\n"
  | LineRaw(e1,e2,e3,e4)-> "line ( (" ^ string_of_texpr e1 ^ "," ^ string_of_texpr e2 ^ ")" ^ "," ^ "(" ^ string_of_texpr e3
                              ^ "," ^ string_of_texpr e4 ^ ") )\n"
  | LinePX(e1, e2, e3)-> "line ( ( " ^ string_of_texpr e1 ^ "," ^ string_of_texpr e2 ^ ") ," ^ string_of_texpr e3 ^ ") \n"
  | For(s1, e1, s2, body) -> "for " ^ string_of_tstmt s1 ^ " ; " ^ string_of_texpr e1 ^ " ; " ^ string_of_tstmt s2 ^ ": \n"
                            ^ ( String.concat "\n\t" (List.map string_of_tstmt body) )
                            ^ "\nend\n"
  | While(e, body) -> "while " ^ string_of_texpr e ^ " :\n" ^ (String.concat "\n\t" (List.map string_of_tstmt body)) ^ "\nend\n"
  | Ifelse(e, succ_tstmt, else_tstmt) -> "if " ^ string_of_texpr e ^ " :\n" ^ (String.concat "\n\t" (List.map string_of_tstmt succ_tstmt)) ^ "\nelse:\n" ^ (String.concat "\n\t" (List.map string_of_tstmt else_tstmt)) ^ "end\n"
  | Return(texpr) -> "return " ^ string_of_texpr texpr ^ "\n"
  | Notexpr -> ""
  | Fdecl(f) -> string_of_fdecl f and
  string_of_fdecl fdecl =
      "fn " ^ fdecl.fname ^ "(" ^
        ( String.concat ", " (List.map (fun s -> string_of_tstmt s) fdecl.args) ) ^
         "):\n" ^
      ( String.concat "" (List.map string_of_tstmt fdecl.body) ) ^
      "\nend\n"

let string_of_tprogram prog =
  String.concat "\n" (List.map string_of_tstmt prog.funcs)
  ^ "\n-----------\n" ^
  String.concat "\n" (List.map string_of_tstmt prog.main)
