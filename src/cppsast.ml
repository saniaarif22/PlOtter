(* converts plotter AST to C++ AST *)
open Tast
open Ast

(* Convert ast expr to Tast expr *)


let convert_to_cppast stmts_list =
let rec convert_to_texpr stmt = function
  | Ast.Literal_Num(v) -> Tast.Literal_Float(v)
  | Ast.Literal_Str(v) -> Tast.Literal_Str(v)
  | Ast.Bool(v)        -> if v = True then Tast.Bool(True) else Tast.Bool(False)
  | Ast.Binop(e1, op, e2) ->
      let te1 = convert_to_texpr stmt e1 ;
      let te2 = convert_to_texpr stmt e2 ;
      Tast.Binop(te1, op, te2)

  | Ast.Id(v)          -> Tast.Id(v)

  in

(* Convert ast stmt to Tast stmt *)
let rec convert_to_tstmt  = function
  | Ast.Expr(e) ->
      let te = convert_to_texpr e in
      Tast.Expr(te)
  | Ast.Passign(v,e1,e2) ->
      let te1 = convert_to_texpr e1 in
      let te2 = convert_to_texpr e2 in
    Sast.Passign(sv, se1, se2)
  | Ast.Assign(e1,e2) ->
    let te1 = convert_to_texpr e1 in
    let te2 = convert_to_texpr e2 in
      Sast.Assign(te1, te1)
  | Ast.Print(e) -> Sast.Print(convert_to_texpr e)
  | Ast.Return(e) -> Sast.Return(convert_to_texpr e)

  in
  List.map (fun s -> convert_to_tstmt s) stmts_list
in
Tast.string_of_tprogram (convert_to_cppast stmt)
