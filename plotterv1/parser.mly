%{ open Ast %}

%token EOL LPAREN RPAREN LBRACK RBRACK
%token PLUS MINUS TIMES DIVIDE MOD ASSIGN NOT
%token EQUAL NEQ LESS GREATER LEQ GEQ
%token AND OR NOT
%token SEMI COMMA COMMENT OF
%token STRING NUM BOOL POINT NONE LIST HASH
%token RETURN IF ELSE FOR WHILE END BREAK CONTINUE THEN FN
%token <float> LITERAL
%token <string> STR
%token <string> ID
%token EOF


%nonassoc ELSE END BREAK CONTINUE

%right ASSIGN
%left AND OR
%left NOT
%left EQUAL NEQ
%left LESS GREATER LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD

%start program
%type < Ast.program> program

%%

program:
  code EOF { $1 }
  
code:
   /* nothing */    { [] }
 | code stmt        { $2 :: $1}

/* =============================================
                    Functions     
   ============================================= */
   
   /* No locals. as variables can be declared at any point */
   fdecl:
        FN ID LPAREN formals_opt RPAREN COLON EOL stmt_list END
        { { fname = $2;
            formals = $4;
            body = List.rev $7 } }
    
    formals_opt:
        /* nothing */ { [] }
        | formal_list { List.rev $1 }
     
    formal_list:
        formal                     { [$1] }
        | formal_list COMMA formal { $3 :: $1 }

    formal:
        data_type ID    { ($1, $2) }
    
/* =============================================
                    Variable     
   ============================================= */
   
    literal:
        | LIT_NUM { Literal_Num($1) }
        | LIT_STR { Literal_Str($1) }
        /* list hash point to be added */
    
    primitive:
        | BOOL      {"bool"}
        | NUM       {"num"}
        | STRING    {"string"}
    
    data_type:
        | primitive { $1 }
        /* Point, List and hash are to be added here */
        
    vdecl:
        | primitive_var_decl EOL { $1 }
    
    primitive_var_decl:
        | primitive ID  { Var_Decl($1, $2) }
        
        
    /*
        For futute to do, multiple variable declaration 
        eg : num a,b,c
        
    id_list:
        ID      {$1}
        | ID COMMA id_list { $1, $3 }

    */
    
/* =============================================
                    Statements     
   ============================================= */

    stmt:
        | func_stmt  { $1 }
        | other_stmt { $1 }
    
    func_stmt:
        | fdecl { $1 }
    
    other_stmt:
        | expr EOL { Expr($1) }
        | ID ASSIGN expr EOL { Assign(Id($1), $3) }
        | RETURN expr EOL { Return($2) }
        | FOR expr COLON EOL stmt_list END
        | vdecl EOL { $1 }
        
    other_stmt_list:
        /* nothing */ { [] }
        | other_stmt_list other_stmt { $2 :: $1 }
    
    stmt_list:
        /* nothing */  { [] }
        | stmt_list stmt { $2 :: $1 }
        
    
/* =============================================
                    Expressions     
   ============================================= */
   
   
   log_expr:  (*OF left*)
  | expr EQUAL  expr { Binop($1, Equal, $3) }
  | expr NEQ  expr { Binop($1, Neq,   $3) }
  | expr LESS  expr { Binop($1, Less,  $3) }
  | expr LEQ   expr { Binop($1, Leq,   $3) }
  | expr GREATER  expr { Binop($1, Greater,  $3) }
  | expr GEQ  expr { Binop($1, Geq,   $3) }
  | log_expr AND log_expr { Binop($1, And, $3) }
  | log_expr OR log_expr { Binop($1, Or, $3) }
  
  (*change*) 
  
  expr: 
  | access_expr { $1 }
  | nacc_expr { $1 }
  
  nacc_expr: /* non access exprs */  
  | expr DOT ID LPAREN actuals_opt RPAREN { MemberCall($1, $3, $5) }
  | LPAREN expr RPAREN { $2 }
  | term               { $1 }

  access_expr:
  | expr LBRACKET expr RBRACKET { Access($1, $3) }
  
  
  term : 
  | term PLUS   term { Binop($1, add,   $3) }
  | term MINUS  term { Binop($1, sub,   $3) }
  | term TIMES  term { Binop($1, mult,  $3) }
  | term DIVIDE term { Binop($1, div,   $3) }
  | term MOD term { Binop($1, Mod,   $3) }
  | atom             { $1 }
  
  atom:
  | LXM              { LITERAL($1) }
  | TRUE             { Boolean(True) }
  | FALSE            { Boolean(False) }
  | lit               { Id($1) }
