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
| Ast.Add -> Tast.Add
| Ast.Less -> Tast.Less
| Ast.Leq -> Tast.Leq
| Ast.Greater -> Tast.Greater
| Ast.Geq -> Tast.Geq
| Ast.And -> Tast.And
| Ast.Or -> Tast.Or
| Ast.Square -> Tast.Square

let convert_to_cppast stmts_list =
  let rec convert_to_texpr  = function
    | Ast.Literal_Num(v) -> Tast.Literal_Float(v)
    | Ast.Literal_Str(v) -> Tast.Literal_Str(v)
    | Ast.Bool(v)        -> if v = True then Tast.Bool(True) else Tast.Bool(False)
    | Ast.Binop(e1, op, e2) ->
        let te1 = convert_to_texpr e1 in
        let te2 = convert_to_texpr e2 in
        let top = match_of op in
        Tast.Binop(te1, top, te2)
    | Ast.Id(v)          -> Tast.Id(v)

  in

  (* Convert ast stmt to Tast stmt *)
  let rec convert_to_tstmt stmt = function
    | Ast.Expr(e) ->
        let te = convert_to_texpr e in
        Tast.Expr(te)
    | Ast.Assign(e1,e2) ->
      let te1 = convert_to_texpr e1 in
      let te2 = convert_to_texpr e2 in
        Tast.Assign(te1, te1)
    | Ast.Print(e) -> Tast.Print(convert_to_texpr e)
    | Ast.Return(e) -> Tast.Return(convert_to_texpr e)

  in
  List.map (fun s -> convert_to_tstmt s) stmts_list
