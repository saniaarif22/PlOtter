open Sast
open Ast

module StringMap = Map.Make(String)

type s_env = 
        {      var_types :  string StringMap.t ref list;
               var_inds : int StringMap.t ref list;
        }
    
let check stmts =  

    let fail msg = (* raise (Failure msg) *)
                    print_string "Error : ";
                    print_string msg;
                    print_string "\n";
                    exit 0
    in
    
    let find_max_index map = 
        let bindings = StringMap.bindings map in
        let rec max cur = function
            | [] -> cur
            | hd :: tl -> if snd hd > cur then max (snd hd) tl else max cur tl
        in
        max 0 bindings
    in
    
    let type_to_str t = match t with 
        | Sast.Num -> "num"
        | Sast.String -> "string"
        | Sast.Bool -> "bool" 
        | Sast.Point -> "point" 
    in
    
    let str_to_type str_typ = function
        | "num" -> Sast.Num
        | "string" -> Sast.String
        | "bool" -> Sast.Bool
        | "point" -> Sast.Point
    in
    (* Setting Environment for Sast *)
    let find_var var map_list =
        let rec finder var = function
            | m :: tl -> 
                (try (StringMap.find var !m)
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

    
    
    let typeof elem = match elem with
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
                    let tp = find_var v env.var_types in
                    if (tp="num") then
                        Sast.Id(v, Sast.Num)
                    else
                        if (tp="string") then
                            Sast.Id(v,Sast.String)
                        else
                            if (tp="point") then
                                Sast.Id(v,Sast.Point)
                            else
                                Sast.Id(v, Sast.Bool)
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
            | Ast.Passign(v,e1,e2) -> 
                let sv = expr env v in
                let se1 = expr env e1 in
                let se2 = expr env e2 in
                let tv = typeof sv in
                let te1 = typeof se1 in
                let te2 = typeof se2 in
                if ( tv = Sast.Point && te2 = Sast.Num && te1 = Sast.Num )
                    then Sast.Passign(sv, se1, se2)
                else fail ("Invalid type assign. cannot assign " ^ (type_to_str te1) ^ "," ^ (type_to_str te2)  ^ " to type " ^ (type_to_str tv))
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
                let tp = find_var id env.var_types in
                if (tp="num") then
                        Sast.Var_Decl(dt, id, Sast.Num)
                    else
                        if (tp="string") then
                            Sast.Var_Decl(dt, id, Sast.String)
                        else
                            Sast.Var_Decl(dt, id, Sast.Bool)
            | Ast.Print(e) -> Sast.Print(expr env e)
            | Ast.LineVar(e1, e2) -> 
                let se1 = expr env e1 in
                let se2 = expr env e2 in
                let te1 = typeof se1 in
                let te2 = typeof se2 in
                if( te1 = Sast.Point && te2 = Sast.Point )
                then Sast.LineVar(se1, se2)
                else fail ("LineVar has to be called with 2 points")
            | Ast.LineRaw(e1, e2, e3, e4) ->
                let se1 = expr env e1 in
                let se2 = expr env e2 in
                let se3 = expr env e3 in
                let se4 = expr env e4 in
                let te1 = typeof se1 in
                let te2 = typeof se2 in
                let te3 = typeof se3 in
                let te4 = typeof se4 in
                if( te1 = Sast.Num && te2 = Sast.Num && te3 = Sast.Num && te4 = Sast.Num )
                then Sast.LineRaw(se1, se2, se3, se4)
                else fail ("LineRaw has to be called with 4 nums")
            | Ast.For(s1, e1, s2, body) ->
                let ss1 = stmt env s1 in
                let se1 = expr env e1 in
                let ss2 = stmt env s2 in
                 
                Sast.For(ss1, se1, ss2, List.map (fun s -> stmt env s) body)
            | Ast.Return(e) -> Sast.Return(expr env e)
            
        in
        List.map (fun s -> stmt env s) stmts_list
    in
    Sast.string_of_tprogram (convert_to_sast stmts sast_env)