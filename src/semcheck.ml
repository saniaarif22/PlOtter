open Sast
open Ast

module StringMap = Map.Make(String)

type s_env = 
        {      var_types :  string StringMap.t ref list;
               var_inds : string StringMap.t ref list;
        }
    
let check stmts=  

    let fail msg = raise (Failure msg)
    in
    let type_to_str t = match t with 
        | Sast.Num -> "num"
        | Sast.String -> "string"
        | Sast.Bool -> "bool" 
    in
    
    let str_to_type str_typ = function
        | "num" -> Sast.Num
        | "string" -> Sast.String
        | "bool" -> Sast.Bool
    in
    (* Setting Environment for Sast *)
    let find_var var map_list =
        let rec finder var = function
            | m :: tl -> 
                (try str_to_type (StringMap.find var !m)
                 with
                 | Not_found -> finder var tl)
            | [] -> raise (Not_found )
        in 
        finder var map_list
    in
    
    (* build default symbol tables: *)
    let sast_env = 
        {      var_types =  [ref StringMap.empty];
               var_inds =  [ref StringMap.empty];
        }

    in
    
    (*Check parts*)

    
    
    let typeof line = match line with
        | Sast.Literal_Num(_,t) -> t
        | Sast.Literal_Str(_,t) -> t
        | Sast.Binop(_,_,_,t) -> t
        | Sast.Id(_,t) -> t
        | Sast.Bool(_,t) -> t
    in
    
    (* COnverting Ast to Sast *)
    (* uses the sast_env and statment list which is recieved as input *)
    let convert_to_sast stmts_list env= 

        let rec expr env = function
            | Ast.Literal_Num(v) -> Sast.Literal_Num(v, Sast.Num)
            | Ast.Literal_Str(v) -> Sast.Literal_Str(v, Sast.String)
            | Ast.Bool(v)        -> Sast.Bool(v,Sast.Bool)
            | Ast.Id(v)          -> 
                (* uses find_var to determine the type of id *)
                (try
                    (*let tp = str_to_type (find_var v env.var_types) in *)
                    Sast.Id(v, find_var v env.var_types)
                 with
                 | Not_found -> fail ("undeclared variable: " ^ v)
                )
            | Ast.Binop(e1, op, e2) -> 
                let se1 = expr env e1 in
                let se2 = expr env e2 in
                let e1_data = typeof se1 in
                let e2_data = typeof se2 in
                (match op with
                | Add | Sub | Mul | Div -> 
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
                            | String -> Sast.Binop(se1, op, se2, Sast.Bool)
                            | _ -> raise(Failure("Incorrect type with String == or != "))
                        )
                    | Bool ->
                         (match e2_data with
                            | Bool -> Sast.Binop(se1, op, se2, Sast.Bool)
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
            | Ast.Expr(e) -> 
                let se = expr env e in
                let tp = typeof se in
                Sast.Expr(se, tp)
            | Ast.Assign(v,e) -> 
                let sv = expr env v in
                let se = expr env e in
                let tv = typeof sv in
                let te = typeof se in
                if ( tv = te )
                then Sast.Assign(sv, se)
                else fail ("Invalid type assign. cannot assign " ^ (type_to_str te) ^ " to " ^ (type_to_str tv))
            | Ast.Var_Decl(dt, id) -> 
                (try 
                ignore (StringMap.find id !(List.hd env.var_types)); 
                    fail ("variable already declared in local scope: " ^ id)
                 with | Not_found -> (List.hd env.var_types) := StringMap.add id dt !(List.hd env.var_types);
                            (List.hd env.var_inds) := StringMap.add id (find_max_index !(List.hd env.var_inds)+1) !(List.hd env.var_inds); 
                      | Failure(f) -> raise (Failure (f) ) 
                );
                Sast.Var_Decl(dt, id, str_to_type dt)
            | Ast.Print(e) -> Sast.Print(expr env e)
            | Ast.Return(e) -> Sast.Return(expr env e)
            
        in
        List.map (fun s -> stmt env s) stmts_list
    in
    Sast.string_of_tprogram (convert_to_sast stmts sast_env)