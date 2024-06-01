open Ast
open Nnf

let rec nnf_to_cnf tree =
  match tree with
  | Operator (Or, left, (Operator (And, Var l, Var r))) ->
    (* G OR (H AND R) <-> (G OR H) AND (G OR R) *)
    let newl = Operator (Or, left, Var r) in
    let newr = Operator (Or, left, Var l) in
    Operator (And, nnf_to_cnf newl, nnf_to_cnf newr)
  | Operator (Or, (Operator (And, Var l, Var r)), right) ->
    (* (G AND H) OR R <-> (G OR R) AND (H OR R) *)
    let newl = Operator (Or, Var l, right) in
    let newr = Operator (Or, Var r, right) in
    Operator (And, nnf_to_cnf newl, nnf_to_cnf newr)
  | Operator (And, left, Boolean true) -> left (* identity and zero laws*)
  | Operator (Or, Boolean false, right) -> right
  | Operator (Or, Operator (And, _, _), Boolean b) -> Boolean b
  | Operator (Or, Boolean b, Operator (And, _, _)) -> Boolean b
  | _ -> tree

let cnf formula =
  Printf.printf "%s -> " formula;
  str_to_tree formula
  |> ast_to_nnf
  |> nnf_to_cnf
  |> ast_to_rpn
  |> print_endline