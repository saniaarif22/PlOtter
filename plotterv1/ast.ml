type binop = Add|Sub|Mul|Div|Mod|Le|Ge|Eq|Neq|Larger|Smaller|And|Or|Square(*binary operators*)
type uop = Uminus|Not|Neg (*uniary operators*)
type typ = Str|Num|Bool|Point|None (*variable types*)
type bind = typ * string

type expr = (*expressions*)
    Binop of expr * binop * expr (*a + 2*)
  | Id of string (*identifiers*)
  | Num of float(*3.2*)
  | Bool of bool (*True*)
  | String of string(*'abcdefg'*)
  | Uniop of uop*expr(*-2 or ~True*)
  | Assign of string*expr (*a=2*)
  | Call of  string * expr list(*function call*)
  | Noexpr

type stmt = (* Statements *)
  Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt list * stmt list* stmt list
  | For of expr * expr * expr * stmt list
  | While of expr * stmt list
  | Var_dec of var_decl
  | Func_dec of func_decl
  | Break
  | Continue
and func_decl = {(*function declaration*)
  return_type : var_req;
  fname : string;
  formals : var_req list;
  body : stmt list;
}

type program = stmt list(*program*)
