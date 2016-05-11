%{ open Ast 
   open Lexing
   open Parsing
  
  let num_errors = ref 0

  let parse_error msg = (* called by parser function on error *)
  let start = symbol_start_pos() in
  let final = symbol_end_pos() in
  Printf.fprintf stdout "Characters: %d..%d: %s\n"
    (start.pos_cnum - start.pos_bol) (final.pos_cnum - final.pos_bol) msg;
        incr num_errors;
  flush stdout;
  exit 0
  %}

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
        | point         { $1 }
        | literal_list  { $1 } 
    
    literal_list:
        | LBRACK list_elements RBRACK { Literal_List($2) }
        | LBRACK list_elements { (parse_error "Syntax error: Left [ is unmatched with right ]."); }
    

    list_elements:
        /* nothing */ { [] }
        | list_content  { List.rev $1 }
        
    list_content:
        expr                    { [$1] }
        | list_content COMMA expr { $3 :: $1 } 
    
    point:
        | LPAREN expr COMMA expr RPAREN     { Point($2, $4) }
        | LPAREN expr COMMA expr { (parse_error "Syntax error: Left ( is unmatched with right )."); }
        | LPAREN expr COMMA expr { (parse_error "Right ) is unmatched with left ( ."); }
        | LPAREN expr expr RPAREN { (parse_error "Missing , ."); }
        | LPAREN expr COMMA RPAREN { (parse_error "Missing y co-od of point ."); }
        | LPAREN COMMA RPAREN { (parse_error "Missing x and y co-od of point ."); }
        | LPAREN COMMA expr RPAREN { (parse_error "Missing x co-od of point ."); }
    
        
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
        | primitive { (parse_error "Missing variable name"); }

    list_decl:
        | LIST primitive ID { List_Decl($2, $3) }
        | LIST ID           { (parse_error "Missing list type"); }
        | LIST primitive    { (parse_error "Missing list name"); }
        
        
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
        | PRINT EOL          { (parse_error "Nothing to print!"); }
        | line EOL           { $1 }
        | fcall EOL          { $1 }
        | RETURN expr EOL    { Return($2) }
        | RETURN EOL          { (parse_error "Nothing to return!"); }
        | vdecl EOL          { $1 }
        | loop_stmt EOL      { $1 }
        | EOL                { Noexpr }

    cond_stmt:
        | IF expr COLON EOL other_stmt_list END            { Ifelse($2, $5, []) }
        | IF expr EOL other_stmt_list END            { (parse_error "Missing colon :"); }
        | IF expr COLON EOL other_stmt_list          { (parse_error "Missing end "); }
        | IF COLON EOL other_stmt_list END           { (parse_error "Missing if condition "); }
        | IF expr COLON EOL other_stmt_list ELSE COLON EOL other_stmt_list END  { Ifelse($2, $5, $9) }
        | IF expr EOL other_stmt_list ELSE COLON EOL other_stmt_list END  { (parse_error "Missing colon : after if "); }
        | IF expr COLON EOL other_stmt_list ELSE EOL other_stmt_list END  { (parse_error "Missing colon : after else "); }
        | IF expr EOL other_stmt_list ELSE COLON EOL other_stmt_list      { (parse_error "Missing end "); }
        | IF EOL other_stmt_list ELSE COLON EOL other_stmt_list END       { (parse_error "Missing if condition"); }

    list_stmt:
        | ID OF APPEND LPAREN expr RPAREN       { Append( Id($1), $5)}
        | ID OF POP LPAREN RPAREN               { Pop( Id($1) ) }
        | ID OF REMOVE LPAREN expr RPAREN       { Remove( Id($1), $5 ) }
        
    list_assign:
        | ID ASSIGN literal_list {Assign(Id($1), $3) }
        | ID literal_list        {(parse_error "Missing assignment operator "); }
        | ID ASSIGN              {(parse_error "Missing element(s) "); }
 
    assign_stmt:
        | ID ASSIGN expr        { Assign(Id($1), $3) }
        | list_assign           { $1 }
    
    
    line:
        | LINE LPAREN ID COMMA ID RPAREN  { LineVar(Id($3), Id($5) )}
        | LINE  ID COMMA ID RPAREN  { (parse_error "Missing left paren ");}
        | LINE LPAREN ID COMMA ID   { (parse_error "Missing right paren ");}
        | LINE LPAREN ID ID RPAREN  { (parse_error "Missing , ");}
        | LINE LPAREN LPAREN expr COMMA expr RPAREN COMMA LPAREN expr COMMA expr RPAREN RPAREN { LineRaw($4, $6, $10, $12) }
        | LINE LPAREN expr COMMA expr RPAREN COMMA LPAREN expr COMMA expr RPAREN RPAREN { (parse_error "Missing left paren ");}
        | LINE LPAREN LPAREN  expr COMMA expr RPAREN COMMA expr COMMA expr RPAREN RPAREN { (parse_error "Missing left paren ");}
        | LINE LPAREN LPAREN expr COMMA expr  COMMA LPAREN expr COMMA expr RPAREN RPAREN { (parse_error "Missing right paren ");}
        | LINE LPAREN LPAREN expr COMMA expr RPAREN COMMA LPAREN expr COMMA expr RPAREN  { (parse_error "Missing right paren ");}
        | LINE LPAREN LPAREN expr  expr RPAREN COMMA LPAREN expr COMMA expr RPAREN RPAREN { (parse_error "Missing , ");}
        | LINE LPAREN LPAREN expr COMMA expr RPAREN  LPAREN expr COMMA expr RPAREN RPAREN { (parse_error "Missing , ");}
        | LINE LPAREN LPAREN expr COMMA expr RPAREN COMMA LPAREN expr  expr RPAREN RPAREN { (parse_error "Missing , ");}
        | LINE LPAREN LPAREN expr COMMA expr RPAREN COMMA ID RPAREN { LinePX($4, $6, Id($9)) }
        | LINE LPAREN expr COMMA expr RPAREN COMMA ID RPAREN { (parse_error "Missing left paren ");}
        | LINE LPAREN LPAREN expr COMMA expr RPAREN COMMA ID { (parse_error "Missing right paren ");}
        | LINE LPAREN LPAREN expr COMMA expr COMMA ID RPAREN { (parse_error "Missing right paren ");}
        | LINE LPAREN LPAREN expr expr RPAREN COMMA ID RPAREN { (parse_error "Missing , ");}
        | LINE LPAREN LPAREN expr COMMA expr RPAREN ID RPAREN { (parse_error "Missing , ");}
        | LINE LPAREN ID COMMA LPAREN expr COMMA expr RPAREN RPAREN { LinePX($6, $8, Id($3)) }
        | LINE  ID COMMA LPAREN expr COMMA expr RPAREN RPAREN { (parse_error "Missing left paren ");}
        | LINE LPAREN ID COMMA  expr COMMA expr RPAREN RPAREN { (parse_error "Missing left paren ");}
        | LINE LPAREN ID  LPAREN expr COMMA expr RPAREN RPAREN { (parse_error "Missing , ");}
        | LINE LPAREN ID COMMA LPAREN expr  expr RPAREN RPAREN { (parse_error "Missing , ");}
        | LINE LPAREN ID COMMA LPAREN expr COMMA expr  RPAREN { (parse_error "Missing right paren ");}

    loop_stmt:
        | FOR assign_stmt SEMI expr SEMI assign_stmt COLON EOL other_stmt_list END { For($2, $4, $6, List.rev $9) }
        | FOR assign_stmt  expr SEMI assign_stmt COLON EOL other_stmt_list END { (parse_error "Missing ; ");}
        | FOR assign_stmt SEMI expr  assign_stmt COLON EOL other_stmt_list END { (parse_error "Missing ; ");}
        | FOR assign_stmt SEMI expr SEMI assign_stmt  EOL other_stmt_list END { (parse_error "Missing : ");}
        | FOR assign_stmt SEMI expr SEMI assign_stmt COLON EOL other_stmt_list { (parse_error "Missing end ");}
        | FOR  SEMI expr SEMI assign_stmt COLON EOL other_stmt_list END { (parse_error "Missing initialization statement ");}
        | FOR assign_stmt SEMI  SEMI assign_stmt COLON EOL other_stmt_list END { (parse_error "Missing condition statement ");}
        | FOR assign_stmt SEMI expr SEMI  COLON EOL other_stmt_list END { (parse_error "Missing increment/decrement statement ");}
        | WHILE expr COLON EOL other_stmt_list END {While($2, List.rev $5)}
        | WHILE expr COLON EOL other_stmt_list END { (parse_error "Missing : ");}
        | WHILE expr COLON EOL other_stmt_list END { (parse_error "Missing end ");}
        | WHILE expr COLON EOL other_stmt_list END { (parse_error "Missing expression in while ");}
        
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
        | FN ID LPAREN args_opt RPAREN COLON EOL stmt_list END EOL
          { Fdecl({ fname = $2;
            args = List.rev $4;
            body = List.rev $8 }) }
        | FN ID LPAREN args_opt RPAREN COLON EOL stmt_list END EOL { (parse_error "Missing colon : "); }
        | FN ID args_opt RPAREN COLON EOL stmt_list END EOL        { (parse_error "Missing left paren "); }
        | FN ID LPAREN args_opt COLON EOL stmt_list END EOL        { (parse_error "Missing right paren "); }
        | FN LPAREN args_opt RPAREN EOL stmt_list END EOL          { (parse_error "Missing function name : "); }

    args_opt:
         { [] }
        | args_list { List.rev $1 }

    args_list:
          arg                       { [$1] }
        | args_list COMMA arg       { $3 :: $1 }
        | args_list arg             { (parse_error "Missing , "); }
        | args_list COMMA           { (parse_error "Missing arg "); }

    arg:
        vdecl_single                { $1 }

    /* Function Call */
    fcall:
        | ID LPAREN fparam RPAREN   { Fcall($1, $3) }

    fparam:
        {[]}
        | expr                  { [$1] }
        | fparam COMMA expr     { $3 :: $1 }
        | fparam  expr          { (parse_error "Missing , "); }
        | fparam COMMA          { (parse_error "Missing expr "); }
        
    
/* =============================================
                    Expressions     
   ============================================= */
  
  expr: 
  | arith_expr          { $1 }
  | log_expr            { $1 }
  | LPAREN expr RPAREN  { $2 }
  | LPAREN  RPAREN      { (parse_error "Missing expression "); }
  | LPAREN expr         { (parse_error "Missing right paren "); }
   
   log_expr:  
  | expr EQUAL  expr       { Binop($1, Equal, $3) }
  | expr NEQ  expr         { Binop($1, Neq,   $3) }
  | expr LESS  expr        { Binop($1, Less,  $3) }
  | expr LEQ   expr        { Binop($1, Leq,   $3) }
  | expr GREATER  expr     { Binop($1, Greater,  $3) }
  | expr GEQ  expr         { Binop($1, Geq,   $3) }
  | expr expr              { (parse_error "Missing Equal/not equal/ lesser/ greater/ lesser eq/ greater eq "); }
  | expr EQUAL             { (parse_error "Missing second  expression "); }
  | expr NEQ               { (parse_error "Missing second  expression "); }
  | expr LESS              { (parse_error "Missing second  expression "); }
  | expr LEQ               { (parse_error "Missing second  expression "); }
  | expr GREATER           { (parse_error "Missing second  expression "); }
  | expr GEQ               { (parse_error "Missing second  expression "); }
  | log_expr AND log_expr  { Binop($1, And, $3) }
  | log_expr OR log_expr   { Binop($1, Or, $3) }
  | log_expr log_expr      { (parse_error "Missing and/ or "); }
  | log_expr OR            { (parse_error "Missing second  expression "); }
  | log_expr AND           { (parse_error "Missing second  expression "); }
  
  
  arith_expr : 
  | LPAREN arith_expr RPAREN     { $2 }
  | LPAREN arith_expr            { (parse_error "Missing right paren "); }
  | LPAREN RPAREN                { (parse_error "Missing expr "); }
  | arith_expr PLUS   arith_expr { Binop($1, Add,   $3) }
  | arith_expr MINUS  arith_expr { Binop($1, Sub,   $3) }
  | arith_expr TIMES  arith_expr { Binop($1, Mul,  $3) }
  | arith_expr DIVIDE arith_expr { Binop($1, Div,   $3) }
  | arith_expr MOD arith_expr    { Binop($1, Mod,   $3) }
  | arith_expr arith_expr        { (parse_error "Missing plus/minus/times/divide/mod "); }
  | arith_expr PLUS              { (parse_error "Missing second arithmetic expression "); }
  | arith_expr MINUS             { (parse_error "Missing second arithmetic expression "); }
  | arith_expr TIMES             { (parse_error "Missing second arithmetic expression "); }
  | arith_expr DIVIDE            { (parse_error "Missing second arithmetic expression "); }
  | arith_expr MOD               { (parse_error "Missing second arithmetic expression "); }
  | list_exprs                { $1 }
  | atom                { $1 }
  
  list_exprs:
  | ID OF LENGTH LPAREN  RPAREN           { Length( Id($1) ) }
  | ID LENGTH LPAREN  RPAREN              { (parse_error "Missing . "); }
  | ID OF LENGTH  RPAREN                  { (parse_error "Missing left paren "); }
  | ID OF LENGTH LPAREN                   { (parse_error "Missing right paren "); }
  | ID OF AT LPAREN expr RPAREN           { Access( Id($1), $5 ) }
  | ID  AT LPAREN expr RPAREN             { (parse_error "Missing of "); }
  | ID OF AT expr RPAREN                  { (parse_error "Missing left paren "); }
  | ID OF AT LPAREN expr                  { (parse_error "Missing right paren "); }
  | ID LBRACK expr RBRACK                 { Access( Id($1), $3 ) }
  | ID LBRACK RBRACK                      { (parse_error "Missing expr "); }
  | ID LBRACK expr                        { (parse_error "Missing right brack "); }
  
 
  
  atom:
  | literal          { $1 }
  | TRUE             { Bool(True) }
  | FALSE            { Bool(False) }
  | ID               { Id($1) }
