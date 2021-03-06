open Sast
open Ast

module StringMap = Map.Make(String)

type s_env = 
        {      var_types: string StringMap.t ref list;
               var_inds : int StringMap.t ref list;
               f_list   : string StringMap.t ref list;
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
        | Sast.Num          -> "num"
        | Sast.String       -> "string"
        | Sast.Bool         -> "bool" 
        | Sast.Point        -> "point" 
        | Sast.ListBool     -> "listbool" 
        | Sast.ListNum      -> "listnum" 
        | Sast.ListString   -> "liststring" 
        | Sast.ListPoint    -> "listpoint" 
        | Sast.List         -> "list" 
        
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
        {      var_types    = [ref StringMap.empty];
               var_inds     = [ref StringMap.empty];
               f_list      =  [ref StringMap.empty];
        }

    in
    
    (*Check parts*)

    
    
    let typeof elem = match elem with
        | Sast.Literal_Num(_,t) -> t
        | Sast.Literal_Str(_,t) -> t
        | Sast.Point(_,_,t) -> t
        | Sast.Literal_List(_,t) -> t
        | Sast.Binop(_,_,_,t) -> t
        | Sast.Id(_,t) -> t
        | Sast.Bool(_,t) -> t
        | Sast.Length(_,t) -> t
        | Sast.Access(_,_,_,t) -> t
    in
    
    (* COnverting Ast to Sast *)
    (* uses the sast_env and statment list which is recieved as input *)
    let convert_to_sast stmts_list env= 

        let rec expr env = function
            | Ast.Literal_Num(v) -> Sast.Literal_Num(v, Sast.Num)
            | Ast.Literal_Str(v) -> Sast.Literal_Str(v, Sast.String)
            | Ast.Point(e1, e2) -> 
                let se1 = expr env e1 in 
                let se2 = expr env e2 in 
                let te1 = typeof se1 in
                let te2 = typeof se2 in
                if( te1=te2 && te1=Sast.Num)
                then Sast.Point(se1, se2, Sast.Point)
                else fail("Point's value should only be of type num.")
            | Ast.Literal_List(v) -> 
                    let tv = List.map (fun s -> expr env s) v in
                    (match tv with 
                    | [] -> Sast.Literal_List(tv, Sast.List)
                    | hd::tl -> 
                        (match (type_to_str (typeof hd)) with
                        | "num"     -> Sast.Literal_List(tv, Sast.ListNum)
                        | "string"  -> Sast.Literal_List(tv, Sast.ListString)
                        | "bool"    -> Sast.Literal_List(tv, Sast.ListBool)
                        | _         -> fail("Lists can contain only bool/string/num")
                        )
                    )
                    
            | Ast.Bool(v)        -> Sast.Bool(v,Sast.Bool)
            | Ast.Id(v)          -> 
                (* uses find_var to determine the type of id *)
                (try
                    let tp = find_var v env.var_types in
                    ( match tp with
                        | "num"         ->  Sast.Id(v, Sast.Num)
                        | "string"      ->  Sast.Id(v,Sast.String)
                        | "point"       ->  Sast.Id(v,Sast.Point)
                        | "bool"        ->  Sast.Id(v, Sast.Bool)
                        | "listnum"     ->  Sast.Id(v, Sast.ListNum)
                        | "liststring"  ->  Sast.Id(v, Sast.ListString)
                        | "listpoint"   ->  Sast.Id(v, Sast.ListPoint)
                        | "listbool"    ->  Sast.Id(v, Sast.ListBool)
                        | _             ->  fail(" Invalid syntax..")
                    )
                 with
                 | Not_found -> fail ("undeclared variable: " ^ v)
                )
            | Ast.Access(v, e) -> 
                let sv = expr env v in
                let se = expr env e in
                let tv = typeof sv in
                let te = typeof se in
                if ( te=Sast.Num )
                then (
                    if (tv = Sast.ListNum)
                    then Sast.Access(sv, se, Sast.ListNum, Sast.Num)
                    else (
                        if (tv= Sast.ListString)
                        then Sast.Access(sv, se, Sast.ListString, Sast.String)
                        else (
                            if (tv= Sast.ListBool)
                            then Sast.Access(sv, se, Sast.ListBool, Sast.Bool)
                            else (
                                if (tv= Sast.ListPoint)
                                then Sast.Access(sv, se, Sast.ListPoint, Sast.Point)
                                else (if (tv= Sast.Point)
                                    then Sast.Access(sv, se, Sast.Point, Sast.Num)
                                    else ( fail("'access' operations can be performed only on List variables. Here its applied on "^ type_to_str tv)
                                        )
                                    )
                                )
                            )
                        )
                    )
                else fail ("The 'index' in list_elem.at(index)  should be of num type only." ^ (type_to_str tv))
            | Ast.Length(v) -> 
                let sv = expr env v in
                let tv = typeof sv in
                if ( (tv = Sast.ListNum) || (tv = Sast.ListPoint) || (tv = Sast.ListString) || (tv = Sast.ListBool))
                then Sast.Length(sv, Sast.Num)
                else fail ("'length()' can be performed only on List variables.")
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
                            | _ -> fail ("Incorrect type " ^ (type_to_str e2_data) ^ " with Num"))
                    | _ -> fail ("Operation on incompatible types")
                    )
                | Equal | Neq  ->
                    (match e1_data with 
                    | Num ->
                        (match e2_data with
                            | Num -> Sast.Binop(se1, op, se2, Sast.Bool)
                            | _ -> fail("Incorrect type with Num == or != ")
                        )
                    | String ->
                         (match e2_data with
                            | String -> Sast.Binop(se1, op, se2, Sast.Bool)
                            | _ -> fail("Incorrect type with String == or != ")
                        )
                    | Bool ->
                         (match e2_data with
                            | Bool -> Sast.Binop(se1, op, se2, Sast.Bool)
                            | _ -> fail("Incorrect type with Bool == or != ")
                        )
                    | _  -> fail("Type which is not num, string or bool cannot be used in equal or neq")
                    (*| Void  -> fail ("Cannot perform binop on void") *)
                    )

                | And | Or  ->
                    if ( (e1_data=Num || e1_data=Bool ) && (e2_data=Num || e2_data=Bool) )
                    then Sast.Binop(se1, op, se2, Sast.Bool)
                    else fail("Incorrect type with 'and' and 'or'")

                | Less | Leq | Greater | Geq ->
                    (match e1_data with 
                    | Num ->
                        (match e2_data with
                            | Num -> Sast.Binop(se1, op, se2, Sast.Bool)
                            | _ -> fail("Incorrect type with Num < or <= or > or >= ")
                        )
                    | String ->
                         (match e2_data with
                            | String -> Sast.Binop(se1, op, se2, Sast.Bool)
                            | _ -> fail ("cannot mix string and  < or <= or > or >= ")
                        )
                    | _ -> fail ("Cannot perform less and grt ops on these types")
                    )
                | Mod | Square ->
                    (match e1_data with 
                    | Num ->
                        (match e2_data with
                            | Num -> Sast.Binop(se1, op, se2, Sast.Bool)
                            | _ -> fail("Incorrect type for mod. both should be num")
                        )
                    | _ -> fail("Mod & square can only be used with num")
                    )
                )
        in

        (* Convert ast stmt to sast stmt *)
        let rec stmt env = function
            | Ast.Noexpr -> Sast.Noexpr
            | Ast.Expr(e) -> 
                let se = expr env e in
                let tp = typeof se in
                Sast.Expr(se, tp)
            | Ast.Passign(v,e1,e) -> 
                let sv = expr env v in
                let se1 = expr env e1 in
                let tv = typeof sv in
                let te1 = typeof se1 in
                if ( tv = Sast.Point && te1 = tv )
                    then Sast.Passign(sv, se1)
                else fail ("Invalid type assign. cannot assign " ^ (type_to_str te1) ^ " to type " ^ (type_to_str tv))
            | Ast.Assign(v,e) -> 
                let sv = expr env v in
                let se = expr env e in
                let tv = typeof sv in
                let te = typeof se in
                if ( tv = te || (te=Sast.List && (tv=Sast.ListNum || tv=Sast.ListString || tv=Sast.ListBool || tv=Sast.ListPoint) ) )
                then Sast.Assign(sv, se)
                else fail ("Invalid type assign. cannot assign " ^ (type_to_str te) ^ " to " ^ (type_to_str tv))
            | Ast.Append(v, e) -> 
                let sv = expr env v in
                let se = expr env e in
                let tv = typeof sv in
                let te = typeof se in
                if ( (tv = Sast.ListNum && te=Sast.Num) || (tv = Sast.ListPoint && te=Sast.Point ) || (tv = Sast.ListString && te=Sast.String) || (tv = Sast.ListBool && te=Sast.Bool))
                then Sast.Append(sv, se)
                else fail ("Invalid type append. cannot append " ^ (type_to_str te) ^ " to " ^ (type_to_str tv))
            | Ast.Remove(v, e) -> 
                let sv = expr env v in
                let se = expr env e in
                let tv = typeof sv in
                let te = typeof se in
                if ( (tv = Sast.ListNum) || (tv = Sast.ListPoint) || (tv = Sast.ListString) || (tv = Sast.ListBool))
                then
                    if ( te=Sast.Num )
                    then Sast.Remove(sv, se)
                    else fail("The 'index' in *.pop(index) should be of num type only. It cannot be of type " ^ (type_to_str te))
                else fail ("'access' operations can be performed only on List variables.")
            | Ast.Pop(v) -> 
                let sv = expr env v in
                let tv = typeof sv in
                if ( (tv = Sast.ListNum) || (tv = Sast.ListPoint) || (tv = Sast.ListString) || (tv = Sast.ListBool))
                then Sast.Pop(sv)
                else fail ("'pop()' can be performed only on List variables.")
            | Ast.Fcall(v, el) -> 
                let sel = List.map (fun s -> expr env s) el in
                (* Check if function is present *)
                Sast.Fcall(v, sel)
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
                            if (tp="point") then
                                Sast.Var_Decl(dt, id, Sast.Point)
                            else
                                Sast.Var_Decl(dt, id, Sast.Bool)
            | Ast.List_Decl(dt, id) -> 
                (try 
                ignore (StringMap.find id !(List.hd env.var_types)); 
                    fail ("variable already declared in local scope: " ^ id)
                 with | Not_found -> (List.hd env.var_types) := StringMap.add id ("list"^dt) !(List.hd env.var_types);
                            (List.hd env.var_inds) := StringMap.add id (find_max_index !(List.hd env.var_inds)+1) !(List.hd env.var_inds); 
                      | Failure(f) -> raise (Failure (f) ) 
                );
                let tp = find_var id env.var_types in
                if (tp="listnum") then
                    Sast.List_Decl(dt, id, Sast.ListNum)
                else
                    if (tp="liststring") then
                        Sast.List_Decl(dt, id, Sast.ListString)
                    else
                        if (tp="listpoint") then
                            Sast.List_Decl(dt, id, Sast.ListPoint)
                        else
                            Sast.List_Decl(dt, id, Sast.ListBool)
            | Ast.Print(e) -> 
                let se1 = expr env e in
                let te1 = typeof se1 in
                Sast.Print(se1)
            | Ast.PrintXY(e1,e2) -> 
                let se1 = expr env e1 in
                let se2 = expr env e2 in
                let te1 = typeof se1 in
                let te2 = typeof se2 in
                if (te2 = Sast.Point)
                then Sast.PrintXY(se1, se2)
                else fail("The second argument of printXY should be of type point")
            | Ast.LineVar(e1, e2) -> 
                let se1 = expr env e1 in
                let se2 = expr env e2 in
                let te1 = typeof se1 in
                let te2 = typeof se2 in
                if( te1 = Sast.Point && te2 = Sast.Point )
                then Sast.LineVar(se1, se2)
                else fail ("LineVar has to be called with 2 points")
            | Ast.LineVarColor(e1, e2,c) -> 
                let se1 = expr env e1 in
                let se2 = expr env e2 in
                let sc  = expr env c in
                let te1 = typeof se1 in
                let te2 = typeof se2 in
                let tc = typeof sc in
                if( te1 = Sast.Point && te2 = Sast.Point && tc=Sast.String )
                then Sast.LineVarColor(se1, se2, sc)
                else fail ("Line has to be called with 2 points and string for color (optional)")
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
            | Ast.LinePX(e1, e2, e3) ->
                let se1 = expr env e1 in
                let se2 = expr env e2 in
                let se3 = expr env e3 in
                let te1 = typeof se1 in
                let te2 = typeof se2 in
                let te3 = typeof se3 in
                if( te1 = Sast.Num && te2 = Sast.Num && te3 = Sast.Point )
                then Sast.LinePX(se1, se2, se3)
                else fail ("Line has to be called with 2 points")
            | Ast.For(s1, e1, s2, body) ->
                let ss1 = stmt env s1 in
                let se1 = expr env e1 in
                let ss2 = stmt env s2 in
                Sast.For(ss1, se1, ss2, List.map (fun s -> stmt env s) body)
            | Ast.While(e, body) ->
                let se = expr env e in
                let te = typeof se in 
                if ( te = Sast.Num || te = Sast.Bool)
                then Sast.While(se, List.map (fun s -> stmt env s) body)
                else fail("The condition in while should give eiether num or bool. Not of type " ^type_to_str te)
            | Ast.Ifelse(e, s1, s2) ->
                let se = expr env e in
                Sast.Ifelse(se, List.map (fun s -> stmt env s) s1, List.map (fun s -> stmt env s) s2)
            | Ast.Return(e) -> Sast.Return(expr env e)
            | Ast.Fdecl(f) -> 
                    let fnEnv = {      
                            var_types   =  [ref StringMap.empty];
                            var_inds    =  [ref StringMap.empty];
                            f_list      =  [ref StringMap.empty];
                    } in
                    let f_name = 
                        (try 
                        ignore (StringMap.find f.fname !(List.hd env.f_list)); 
                            fail ("Function already declared in local scope: " ^ f.fname)
                         with | Not_found -> (List.hd env.f_list) := StringMap.add f.fname "function" !(List.hd env.f_list);
                              | Failure(f) -> raise (Failure (f) ) 
                        );
                        
                    
                    in
                    let fargs = List.map (fun s -> stmt fnEnv s) f.args in
                    let fstms = List.map (fun s -> stmt fnEnv s) f.body in
                    Sast.Fdecl({
                        fname = f.fname;
                        args  = fargs;
                        body  = fstms;
                    })
        in
        List.map (fun s -> stmt env s) stmts_list
    in
    Sast.string_of_tprogram (convert_to_sast stmts sast_env)