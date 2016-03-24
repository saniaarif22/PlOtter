%{
open Ast
(*f t returns the default initial value of type t*)
%}
/*tokens got from scanner*/
%token LPAREN RPAREN LBRACK RBRACK
%token PLUS MINUS TIMES DIVIDE MOD ASSIGN NOT
%token EQ LE SE NEQ LARGER SMALLER
%token AND OR NOT
%token SEMICOLON COMMA COMMENT DOLLAR
%token INTEGER STRING FLOAT BOOL LINE CIRCLE POINT VOID
%token RETURN IF ELSE ELIF FOR WHILE END BREAK CONTINUE DO THEN
%token PASS
%token LIB
%token <int> INT
%token <float> FLO
%token <string> STR
%token <string> ID
%token <bool> BOL
%token EOF


%right ASSIGN
%left AND OR
%left NOT
%left EQ NEQ
%left LE SE LARGER SMALLER
%left PLUS MINUS
%left UMINUS
%left TIMES DIVIDE MOD

%start program
%type < Ast.program> program

%%

/* Our program allows variable and function declaration any time*/
program:
  o_stmt {[$1]}
 | program o_stmt  { $2::$1 }

/*variable declaration*/
var_type:
   INTEGER {Int}
 | STRING  {Str}
 | FLOAT   {Flo}
 | BOOL    {Bol}
 | LINE    {Lin}
 | CIRCLE  {Cir}
 | POINT   {Poi}
 | VOID {Void}
v_decl:/*3 type of declarations*/
   var_type ID SEMICOLON { Var_dec({v_type=$1;v_name=$2;init_value=f $1;}) }/*int a;*/
  | var_type ID ASSIGN expr SEMICOLON { Var_dec({v_type=$1;v_name=$2;init_value=$4;}) }/*int a=0*/
/*function declaration*/
f_decl:
   var_type ID LPAREN formals_opt RPAREN o_stmt_list END { Func_dec({ return_type={v_type=$1;v_name="";}; fname = $2; formals = $4; body = List.rev $6 }) }
/*formals*/
formals_opt:
  /* nothing */ { [] }
  | formal_list { List.rev $1 }
formal_list:
    formal                   { [$1] }
  | formal_list COMMA formal { $3 :: $1 }

formal:
   var_type ID { {v_type=$1;v_name=$2}}
/*expressions*/
expr:
  expr PLUS    expr { Binop($1, Add, $3) }
| expr MINUS   expr { Binop($1, Sub, $3) }
| expr TIMES   expr { Binop($1, Mul, $3) }
| expr DIVIDE  expr { Binop($1, Div, $3) }
| expr MOD     expr { Binop($1, Mod, $3) }
| expr EQ      expr { Binop($1, Eq,  $3) }
| expr NEQ     expr { Binop($1, Neq, $3) }
| expr LE      expr { Binop($1, Le,  $3) }
| expr SE      expr { Binop($1, Se,  $3) }
| expr LARGER  expr { Binop($1, Larger, $3) }
| expr SMALLER expr { Binop($1, Smaller, $3) }
| expr AND     expr { Binop($1, And, $3) }
| expr OR      expr { Binop($1, Or, $3) }
| ID ASSIGN expr {Assign($1,$3)}
| ID LPAREN actuals_opt RPAREN { Call($1, $3) }
| ID DOLLAR ID { Property($1,$3)}
| ID DOLLAR ID LPAREN actuals_opt RPAREN{Property_Call($1,$3,$5)}
| LIB DOLLAR ID LPAREN actuals_opt RPAREN{Library_Call("library",$3,$5)}
| LPAREN expr RPAREN { $2 }
| NOT expr {Uniop(Not,$2)}
| MINUS expr %prec UMINUS{Uniop(Uminus,$2)}
| INT          { Integer($1) }
| STR          { String($1)  }
| FLO          { Float($1)   }
| BOL          { Bool($1)    }
| ID           { Id($1)      }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

/*actuals in function_all*/
actuals_opt:
/* nothing */ { [] }
| actuals_list { List.rev $1 }

actuals_list:
expr { [$1] }
| actuals_list COMMA expr { $3 :: $1 }

/* statements inside loops, ifs*/
stmt:
  expr SEMICOLON { Expr($1) }
| RETURN expr_opt SEMICOLON { Return($2) }
| BREAK SEMICOLON {Break}
| CONTINUE SEMICOLON {Continue}
| IF expr THEN stmt_list END {If($2,$4,[Pass],[Pass])}
| IF expr THEN stmt_list estmt_list END {If($2,$4,$5,[Pass])}
| IF expr THEN stmt_list estmt_list ELSE stmt_list END {If($2,$4,$5,$7)}
| IF expr THEN stmt_list ELSE stmt_list END {If($2,$4,[Pass],$6)}
| FOR expr_opt SEMICOLON expr_opt SEMICOLON expr_opt DO stmt_list END {For($2,$4,$6,$8)}
| WHILE expr DO stmt_list  END {While($2,$4)}
| PASS SEMICOLON{Pass}
| v_decl{$1}
stmt_list:
   stmt_list stmt { $2 :: $1 }
  | stmt {[$1]}



 /*o_stmt allows variable and function declaration*/
o_stmt:
   stmt {$1}
  | f_decl {$1}


o_stmt_list:
   o_stmt_list o_stmt { $2 :: $1 }
  | o_stmt{[$1]}


/* else-if statement*/
estmt:
   ELIF expr THEN stmt_list { Elif($2,$4)}

estmt_list:
	estmt {[$1]}
  | estmt_list estmt {$2::$1}
