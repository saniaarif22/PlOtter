open Sast

module StringMap = Map.Make(String)

    
let check stmts=  

    let fail msg = raise (Failure msg)
    in
    
    (* Setting Environment for Sast *)
    let find_var var map_list =
        let rec finder var = function
            | m :: tl -> 
                (try StringMap.find var !m
                 with
                 | Not_found -> finder var tl)
            | [] -> raise (Not_found )
        in 
        finder var map_list
    in
    
    let sast_env = 
        (* build default symbol tables: *)
        {      var_types = [ref StringMap.empty];
               var_inds = [ref StringMap.empty];
               return_type = Sast.Void
        }
    in
    
    (*Check parts*)
    
    let str_to_type = function
        | "num" -> Sast.Num
        | "string" -> Sast.String
        | "bool" -> Sast.Bool
    in
    
    let type_to_str = function
        | Sast.Num -> "num"
        | Sast.String -> "string"
        | Sast.Bool -> "bool" 
    in
    
    let typeof line = match line with
        | Literal_Num(_,t) -> t
        | Literal_Str(_,t) -> t
        | Binop(_,_,_,t) -> t
        | Id(_,t) -> t
        | Bool(_,t) -> t
    in
    
    (* COnverting Ast to Sast *)
    (* uses the sast_env and statment list which is recieved as input *)
    let convert_to_sast stmts_list env= 

        let rec expr env = function
            | Ast.Literal_Num(v) -> Sast.Literal_Num(v, Sast.Num)
            | Ast.Literal_Str(v) -> Sast.Literal_Num(v, Sast.Str)
            | Ast.Bool(v)        -> Sast.Bool(v,Sast.Bool)
            | Ast.Id(v) -> 
                (try
                    Sast.Id(v, find_var v env.var_types) 
                    (* uses find_var to determine the type of id *)
                 with
                 | Not_found -> fail ("undeclared variable: " ^ v)
                )

            | Ast.Binop(e1, op, e2) -> 
                let se1 = expr env e1 in
                let se2 = expr env e2 in
                let e1_data = typeof se1 in
                let e2_data = typeof se2 in
                (match op with
                | Add | Sub | Mult | Div -> 
                    (match e1_data with 
                    | Num ->
                        (match e2_data with
                            | Num -> Sast.Binop(se1, op, se2, Sast.Num)
                            | String ->  fail ("Cannot Add Num and string")
                            | Bool -> fail ("Cannot Add Bool")
                            | _ -> fail ("Incorrect type with Num"))
                    | _ -> fail ("Operation on incompatible types")
                    )
                | Equal | Neq ->
                    (match e1_data with 
                    | Num ->
                        (match e2_data with
                            | Num -> Sast.Binop(se1, op, se2, Sast.Bool)
                            | _ -> raise(Failure("Incorrect type with Num == or != "))
                        )
                    | String ->
                         (match e2_data with
                            | Equal | Neq -> Sast.Binop(se1, op, se2, Sast.Bool)
                            | _ -> raise(Failure("Incorrect type with String == or != "))
                        )
                    | Bool ->
                         (match e2_data with
                            | Equal | Neq -> Sast.Binop(se1, op, se2, Sast.Bool)
                            | _ -> raise(Failure("Incorrect type with Bool == or != "))
                        )
                    (*| Void  -> fail ("Cannot perform binop on void") *)
                    )

                | Less | Leq | Greater | Geq ->
                    (match e1_data with 
                    | Num ->
                        (match e2_data with
                            | Num -> Sast.Binop(se1, op, se2, Sast.Bool)
                            | _ -> raise(Failure("Incorrect type with Num < or <= or > or >= "))
                        )
                    | String ->
                         (match e2_data with
                            | String -> Sast.Binop(se1, op, se2, Sast.Bool)
                            | _ -> fail ("cannot mix string and  < or <= or > or >= ")
                        )
                    | _ -> fail ("Cannot perform less and grt ops on these types")
                    )
                )
        
        in

        (* Convert ast stmt to sast stmt *)
        let rec stmt env = function
            | Ast.Expr(e) -> Sast.Expr(expr env e)
            | Ast.Var_Decl(dt, id) -> 
                (try 
                ignore (StringMap.find id !(List.hd env.var_types)); 
                    fail ("variable already declared in local scope: " ^ id)
                 with | Not_found -> (List.hd env.var_types) := StringMap.add id (str_to_type dt) !(List.hd env.var_types); (* add type map *)
                            (List.hd env.var_inds) := StringMap.add id (find_max_index !(List.hd env.var_inds)+1) !(List.hd env.var_inds); (* add index to the map *)
                      | Failure(f) -> raise (Failure (f) ) 
                );
                Sast.Var_Decl(dt, id, str_to_type dt)
            | Ast.Assign(v,e) -> 
                let sv = expr env v in
                let se = expr env e in
                let tv = typeof sv in
                let te = typeof se in
                if not ( tv = te )
                then fail ("Invalid type assign. cannot assign " ^ (type_to_str te) ^ " to " ^ (type_to_str tv))
                else Sast.Assign(sv,se)
            | Ast.Print(e) -> Sast.Print(expr env e)
            | Ast.Return(e) -> Sast.Return(expr env e)

        in

        List.map (fun s -> stmt env s) stmts_list
        
        
    in
    
    Sast.string_of_tprogram (convert_to_sast stmts sast_env)
   
   
   
   (*
    
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
    
    *)
    