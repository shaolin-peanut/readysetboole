open Ast
open Nnf

(*
    Identity law:
        x ∧ true = x
        x ∨ false = x
    Zero law:
        x ∧ false = false
        x ∨ true = true
 *)
 let identity_zero tree is_cnf =
  match tree, is_cnf with
  (* CNF *)
  | Operator (And, left, Boolean true), 'c' -> left
  | Operator (Or, Boolean false, right), 'c' -> right
  | Operator (And, _, Boolean false), 'c' -> Boolean false
  | Operator (Or, _, Boolean true), 'c' -> Boolean true

  (* DNF *)
  | Operator (Or, left, Boolean false), 'd' -> left
  | Operator (And, Boolean true, right), 'd' -> right
  | Operator (Or, _, Boolean true), 'd' -> Boolean true
  | Operator (And, Boolean false, _), 'd' -> Boolean false

  | _ -> tree

let rec distr_or_over_and tree =
  match tree with
  (* distribute or over and *)
  | Operator (And, left, (Operator (Or, Var l, Var r))) ->
    let newl = Operator (And, left, Var r) in
    let newr = Operator (And, left, Var l) in
    Operator (Or, distr_or_over_and newl, distr_or_over_and newr)
  | Operator (And, (Operator (Or, Var l, Var r)), right) ->
    let newl = Operator (And, Var l, right) in
    let newr = Operator (And, Var r, right) in
    Operator (Or, distr_or_over_and newl, distr_or_over_and newr)
  | _ -> tree

let rec distr_and_over_or tree =
  match tree with
  | Operator (Or, left, (Operator (And, Var l, Var r))) ->
    (* G OR (H AND R) <-> (G OR H) AND (G OR R) *)
    let newl = Operator (Or, left, Var r) in
    let newr = Operator (Or, left, Var l) in
    Operator (And, distr_and_over_or newl, distr_and_over_or newr)
  | Operator (Or, (Operator (And, Var l, Var r)), right) ->
    (* (G AND H) OR R <-> (G OR R) AND (H OR R) *)
    let newl = Operator (Or, Var l, right) in
    let newr = Operator (Or, Var r, right) in
    Operator (And, distr_and_over_or newl, distr_and_over_or newr)
  | _ -> tree

let nnf_to_cnf tree =
  let dist = distr_and_over_or tree in
  identity_zero dist 'c'

let nnf_to_dnf tree =
  let dist = distr_or_over_and tree in
  identity_zero dist 'd'
      
let cnf formula =
  Printf.printf "%s -> " formula;
  str_to_tree formula
  |> ast_to_nnf
  |> nnf_to_cnf
  |> ast_to_rpn
  |> print_endline