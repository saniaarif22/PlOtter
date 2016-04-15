open Sast

module StringMap = Map.Make(String)


    
let check stmts=  

    let fail msg = raise (Failure msg)
    in
    
    (* Setting Environment for Sast *)
    let sast_env = 
        (* build default symbol tables: *)
        {      var_types = [ref StringMap.empty];
               var_inds = [ref StringMap.empty];
               return_type = Sast.Void
        } in

    (* COnverting Ast to Sast *)
    (* uses the sast_env and statment list which is recieved as input *)
    let convert_to_sast = 

        let rec expr env = function
        | Ast.Literal_Num(v) -> Sast.Literal_Num(v, Sast.Num)
        | Ast.Literal_Str(v) -> Sast.Literal_Num(v, Sast.Str)
        | Ast.Bool(v)        -> Sast.Bool(v,Sast.Bool)
        
    let typeof line = match line with
        | Literal_Num(_,t) -> t
        | Literal_Str(_,t) -> t
        | Binop(_,_,_,t) -> t
        | Id(_,t) -> t
        | Bool(_,t) -> t
    in
    
    let rec check_expr expr = match expr with
        | Literal_Num(_,t) -> t
        | Literal_Str(_,t) -> t
        | Id(_,t) -> t
        | Bool(_,t) -> t
        | Binop (e1,op,e2, t) -> 
                let t1 = check_expr e1 in
                let t2 = check_expr e2 in
                if t1 = t2 then t1
                else fail "Types dont match.."
        
    in
    let check_stmt st = match st with
        | Sast.Expr(expr, t) -> if (check_expr expr) = t then else fail "Expr type mismatch"
    in
    
    
    
    
    List.iter check_stmt stmts