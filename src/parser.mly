%{ open Ast %}

%token EOL LPAREN RPAREN LBRACK RBRACK
%token PLUS MINUS TIMES DIVIDE MOD ASSIGN
%token EQUAL NEQ LESS GREATER LEQ GEQ
%token AND OR NOT
%token SEMI COMMA COMMENT OF COLON
%token STRING NUM BOOL POINT NONE LIST HASH 
%token APPEND POP REMOVE AT LENGTH OF
%token TRUE FALSE
%token RETURN IF ELSE FOR WHILE END BREAK CONTINUE THEN FN
%token PRINT
%token LINE
%token <float> LIT_NUM
%token <string> LIT_STR
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
  stmt EOF { $1 }
  
stmt:
    /* nothing */       { { funcs=[]; main=[] } }
    | func_stmt stmt    { { funcs = $1::$2.funcs; main= $2.main} }    
    | other_stmt stmt   { { funcs = $2.funcs; main= $1::$2.main} }
   
   

    
/* =============================================
                    Variable     
   ============================================= */
   
    literal:
        | LIT_NUM       { Literal_Num($1) }
        | LIT_STR       { Literal_Str($1) }
        | literal_list  { $1 } 
        
    literal_list:
        | LBRACK list_elements RBRACK { Literal_List($2) }
    
    list_elements:
        /* nothing */ { [] }
        | list_content  { List.rev $1 }
        
    list_content:
        expr                    { [$1] }
        | list_content COMMA expr { $3 :: $1 } 
    

    primitive:
        | BOOL      {"bool"}
        | NUM       {"num"}
        | STRING    {"string"}
        | POINT     {"point"}

    /*non_primitive:
        | LIST primitive */
    
    data_type:
        | primitive { $1 }
        /*| non_primitive { $1 }*/
        /* Point, List and hash are to be added here */
        
    vdecl:
        | vdecl_single      { $1 }
    
    /* only to be used to declare one variable */
    /* Reusability in functions */
    vdecl_single:
        | primitive_var_decl { $1 }
        | list_decl { $1 }
    
    primitive_var_decl:
        | primitive ID  { Var_Decl($1, $2) }

    list_decl:
        | LIST primitive ID { List_Decl($2, $3) }
        
        
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

    
    func_stmt:
        | fdecl { $1 }

    other_stmt:
        | expr EOL           { Expr($1) }
        | cond_stmt EOL      { $1 }
        | list_stmt EOL      { $1 }
        | assign_stmt EOL    { $1 }
        | PRINT expr EOL     { Print($2) }
        | line EOL           { $1 }
        | fcall EOL          { $1 }
        | RETURN expr EOL    { Return($2) }
        | vdecl EOL          { $1 }
        | loop_stmt EOL           { $1 }
        | EOL                { Noexpr }

    cond_stmt:
        | IF expr COLON EOL other_stmt_list END            { Ifelse($2, $5, []) }
        | IF expr COLON EOL other_stmt_list ELSE COLON EOL other_stmt_list END  { Ifelse($2, $5, $9) }

    list_stmt:
        | ID OF APPEND LPAREN expr RPAREN       { Append( Id($1), $5)}
        | ID OF POP LPAREN RPAREN               { Pop( Id($1) ) }
        | ID OF REMOVE LPAREN expr RPAREN       { Remove( Id($1), $5 ) }
        | ID OF AT LPAREN expr RPAREN           { Access( Id($1), $5 ) }
        | ID LBRACK expr  RBRACK                { Access( Id($1), $3 ) }
 
    list_assign:
        | ID ASSIGN literal_list {Assign(Id($1), $3) }
 
    assign_stmt:
        | ID ASSIGN LPAREN expr COMMA expr RPAREN { Passign(Id($1),$4,$6)}
        | ID ASSIGN expr        { Assign(Id($1), $3) }
        | list_assign           { $1 }
    
    
    line:
        | LINE LPAREN ID COMMA ID RPAREN  { LineVar(Id($3), Id($5) )}
        | LINE LPAREN LPAREN expr COMMA expr RPAREN COMMA LPAREN expr COMMA expr RPAREN RPAREN { LineRaw($4, $6, $10, $12) }
        | LINE LPAREN LPAREN expr COMMA expr RPAREN COMMA ID RPAREN { LinePX($4, $6, Id($9)) }
        | LINE LPAREN ID COMMA LPAREN expr COMMA expr RPAREN RPAREN { LinePX($6, $8, Id($3)) }

    loop_stmt:
        | FOR assign_stmt SEMI expr SEMI assign_stmt COLON EOL other_stmt_list END { For($2, $4, $6, List.rev $9) }
        | WHILE expr COLON EOL other_stmt_list END {While($2, List.rev $5)}
        
    other_stmt_list:
        { [] }
        | other_stmt_list other_stmt { $2 :: $1 }
    
    stmt_list:
         { [] }
        | stmt_list other_stmt { $2 :: $1 }

/* =============================================
                    Functions     
   ============================================= */
   
   /* No locals. as variables can be declared at any point */

    fdecl:
        FN ID LPAREN args_opt RPAREN COLON EOL stmt_list END EOL
        { Fdecl({ fname = $2;
            args = $4;
            body = List.rev $8 }) }

    args_opt:
         { [] }
        | args_list { List.rev $1 }

    args_list:
          arg                       { [$1] }
        | args_list COMMA arg       { $3 :: $1 }

    arg:
        vdecl_single                { $1 }

    /* Function Call */
    fcall:
        | ID LPAREN fparam RPAREN   { Fcall(Id($1), $3) }

    fparam:
        | expr                  { [$1] }
        | fparam COMMA expr     { $3 :: $1 }
        
    
/* =============================================
                    Expressions     
   ============================================= */
  
  expr: 
  | arith_expr          { $1 }
  | log_expr            { $1 }
  | LPAREN expr RPAREN  { $2 }
   
   log_expr:  
  | expr EQUAL  expr { Binop($1, Equal, $3) }
  | expr NEQ  expr { Binop($1, Neq,   $3) }
  | expr LESS  expr { Binop($1, Less,  $3) }
  | expr LEQ   expr { Binop($1, Leq,   $3) }
  | expr GREATER  expr { Binop($1, Greater,  $3) }
  | expr GEQ  expr { Binop($1, Geq,   $3) }
  | log_expr AND log_expr { Binop($1, And, $3) }
  | log_expr OR log_expr { Binop($1, Or, $3) }
  
  
  arith_expr : 
  | arith_expr PLUS   arith_expr { Binop($1, Add,   $3) }
  | arith_expr MINUS  arith_expr { Binop($1, Sub,   $3) }
  | arith_expr TIMES  arith_expr { Binop($1, Mul,  $3) }
  | arith_expr DIVIDE arith_expr { Binop($1, Div,   $3) }
  | arith_expr MOD arith_expr    { Binop($1, Mod,   $3) }
  | list_len_expr                { $1 }
  | atom                { $1 }
  
  list_len_expr:
  | ID OF LENGTH LPAREN  RPAREN           { Length( Id($1) ) }
  atom:
  | literal          { $1 }
  | TRUE             { Bool(True) }
  | FALSE            { Bool(False) }
  | ID               { Id($1) }
