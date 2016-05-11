(* converts plotter AST to C++ AST *)
open Tast
open Ast

(* Convert ast expr to Tast expr *)
let match_of e = match e with
| Ast.Add -> Tast.Add
| Ast.Sub -> Tast.Sub
| Ast.Mul -> Tast.Mul
| Ast.Div -> Tast.Div
| Ast.Mod -> Tast.Mod
| Ast.Equal -> Tast.Equal
| Ast.Neq -> Tast.Neq
| Ast.Less -> Tast.Less
| Ast.Leq -> Tast.Leq
| Ast.Greater -> Tast.Greater
| Ast.Geq -> Tast.Geq
| Ast.And -> Tast.And
| Ast.Or -> Tast.Or
| Ast.Square -> Tast.Square

let convert_to_cppast stmts_list =
  let rec convert_to_texpr  = function
    | Ast.Literal_Num(v) -> Tast.Literal_Num(v)
    | Ast.Literal_Str(v) -> Tast.Literal_Str(v)
    | Ast.Literal_List(e) ->
      let te = List.map (fun s -> convert_to_texpr s) e in
      Tast.Literal_List(te)
    | Ast.Bool(v)        -> if v = True then Tast.Bool(True) else Tast.Bool(False)
    | Ast.Length(v) -> Tast.Length(convert_to_texpr v)
    | Ast.Binop(e1, op, e2) ->
        let te1 = convert_to_texpr e1 in
        let te2 = convert_to_texpr e2 in
        let top = match_of op in
        Tast.Binop(te1, top, te2)
    | Ast.Id(v)          -> Tast.Id(v)
    | Ast.Access(v,e) ->
        let texpr_acc = convert_to_texpr e in
        let tv = convert_to_texpr v in
        Tast.Access(tv, texpr_acc)
    | Ast.Point(e1, e2) -> 
        let te1 = convert_to_texpr e1 in
        let te2 = convert_to_texpr e2 in
        Tast.Point(te1, te2) 

  in

  (* Convert ast stmt to Tast stmt *)
  let rec convert_to_tstmt stmt = function
    | Ast.Expr(e) ->
        let texpr = convert_to_texpr e in
        Tast.Texpr(texpr)
    | Ast.Var_Decl(t,e) ->Tast.Var_Decl(t,e)
    | Ast.List_Decl(t,id) -> Tast.List_Decl(t, id)
    | Ast.Passign(v,e1,e2) -> Tast.Noexpr
    | Ast.Assign(v,e) ->
      let te1 = convert_to_texpr e in
      let tv = convert_to_texpr v in
        Tast.Assign(tv, te1)
    | Ast.Print(e) -> Tast.Print(convert_to_texpr e)
    | Ast.Append(v,e) ->
      let texpr_app = convert_to_texpr e in
      let tv = convert_to_texpr v in
      Tast.Append(tv, texpr_app)
    | Ast.Remove(v,e) ->
        let texpr_rm = convert_to_texpr e in
        let tv = convert_to_texpr v in
        Tast.Remove(tv, texpr_rm)
    | Ast.Pop(e) -> Tast.Pop(convert_to_texpr e)
    | Ast.LineVar(e1,e2) ->
      let texpr1 = convert_to_texpr e1 in
      let texpr2 = convert_to_texpr e2 in
      Tast.LineVar(texpr1, texpr2)
    | Ast.LineRaw(e1,e2,e3,e4) ->
      let texpr1 = convert_to_texpr e1 in
      let texpr2 = convert_to_texpr e2 in
      let texpr3 = convert_to_texpr e3 in
      let texpr4 = convert_to_texpr e4 in
      Tast.LineRaw(texpr1, texpr2,texpr3,texpr4)
    | Ast.For(s1, e1, s2, body) ->
      let tstmt1 = convert_to_tstmt stmt s1 in
      let texpr1 = convert_to_texpr e1 in
      let tstmt2 = convert_to_tstmt stmt s2 in
      let tbody = List.map (fun s -> convert_to_tstmt stmt s) body in
      Tast.For(tstmt1, texpr1,tstmt2,tbody)
    | Ast.While(e,body) ->
      let texpr = convert_to_texpr e in
      let tbody = List.map (fun s -> convert_to_tstmt stmt s) body in
      Tast.While(texpr,tbody)

    | Ast.Ifelse(e, succ_stmt, else_stmt) ->
      let texpr = convert_to_texpr e in
      let tsucc_stmt = List.map (fun s -> convert_to_tstmt stmt s) succ_stmt in
      let telse_stmt = List.map (fun s -> convert_to_tstmt stmt s) else_stmt in
      Tast.Ifelse(texpr, tsucc_stmt, telse_stmt)
    | Ast.Return(e) -> Tast.Return(convert_to_texpr e)
    | Ast.Fdecl(f) ->
        let f_name = f.fname in
        let f_args = List.map (fun s -> convert_to_tstmt stmt s) f.args in
        let f_body = List.map (fun s -> convert_to_tstmt stmt s) f.body in
      Tast.Fdecl({
                fname = f_name;
                args  = f_args;
                body  = f_body;
                })
    | Ast.Fcall(f, el) ->
        let tel = List.map (fun s -> convert_to_texpr s) el in
        Tast.Fcall(f, tel)
    | Ast.PrintXY (e1, e2) ->
        let te1 = convert_to_texpr e1 in
        let te2 = convert_to_texpr e2 in
        Tast.PrintXY(te1, te2)
    | Ast.LinePX(e1, e2, e3) -> 
        let te1 = convert_to_texpr e1 in
        let te2 = convert_to_texpr e2 in
        let te3 = convert_to_texpr e3 in
        Tast.LinePX(te1, te2, te3)
    | Ast. Noexpr -> Tast.Noexpr      

  in
  List.map (fun s -> convert_to_tstmt s) stmts_list;