(* open Nnf *)
(* open Cnf *)
(* open Truthtable *)
open Ast

type assignment = (char * bool)

let rec contains_unsat = function
  | [] -> false
  | hd :: _ when hd = Status UNSAT -> true
  | _ :: tl -> contains_unsat tl

let rec all_sat = function
  | [] -> true
  | hd :: _ when hd = Status UNSAT -> false
  | hd :: tl when hd = Status SAT -> all_sat tl
  | _ -> false
  
  let rec get_literals clauses acc =
  let sacc acc v =
    if List.exists (fun x -> x = v) acc then acc
    else v :: acc
  in
  match clauses with
  | Var v :: tl ->
    get_literals tl (sacc acc v)
  | UnaryOperator (_, Var v) :: tl ->
    get_literals tl (sacc acc v)
  | Operator (Or, Var l, Var r) :: tl ->
    let left = sacc acc l in
    let right = sacc left r in
    get_literals tl right
  | _ -> acc

let combine_clauses clauses1 clauses2 =
  List.concat (
    List.map (fun c1 ->
      List.map (fun c2 -> c1 @ c2) clauses2) clauses1
    )

(* write a function that turns all the operands of
    nested ORs in a list, there must be a simple way*)

(* ==== attempt to do nSAT instead of 2SAT only *)

(* let rec cnf_to_clauses tree =
  match tree with
  | Var v -> [[Var v]]
  | UnaryOperator (Not, Var v) -> [[UnaryOperator (Not, Var v)]]
  | Operator (And, l, r) ->
    let clauses_l = cnf_to_clauses l in
    let clauses_r = cnf_to_clauses r in
    clauses_l @ clauses_r
  | Operator (Or, l, r) ->
    let clauses_l = cnf_to_clauses l in
    let clauses_r = cnf_to_clauses r in
    combine_clauses clauses_l clauses_r
  | _ -> failwith "Invalid CNF tree structure"

let example_tree =
  Operator (And,
    Operator (Or, Var 'A', UnaryOperator (Not, Var 'B')),
    Operator (Or, Var 'C', Var 'D')
  )

let clauses = cnf_to_clauses example_tree *)

(* ======= *)
(* 
let rec cnf_to_clauses tree =
  match tree with
  | Operator (And, Var left, Var right) :: tl ->
    [left;right] :: cnf_to_clauses tl
  | Var v :: tl -> [v] :: cnf_to_clauses tl
  | Operator (And, l, r) :: tl ->
    let left = cnf_to_clauses l in
    let right = cnf_to_clauses r in
    (List.concat left right) :: cnf_to_clauses tl
  | Operator (Or, Var left, Var right)
  | _ -> [] *)
(* 
let rec propagate v clauses =
  match clauses with
  | [] -> []
  | clause :: rest ->
    match clause with
      | Operator (Or, UnaryOperator (Not, v), other) -> [other] :: propagate v rest
      | Operator (Or, Var v, _) -> propagate (Var v) rest
      | _ -> [clause] :: propagate v rest

let rec unit_propagation clauses units =
  match clauses with
  | [] -> []
  | clause :: rest ->
    match clause with
    | Var v -> [clause] :: propagate (Var v) clauses
    | UnaryOperator (_, _)-> [clause] :: propagate clause clauses
    | _ -> [clause] :: unit_propagation rest units *)

(* let rec simplify clauses var value =
  let exp_or l r tl =
    let left = if l = var then value else l in
    let right = if r = var then value else l in
    Operator (Or, left, right) :: tl
  in
  match clauses with
  | [] -> []
  | variable :: tl when variable = var ->
    simplify (value :: tl) var value
  | UnaryOperator (_, v) :: tl when v = var->
    simplify (value :: tl) var value
  | Operator (Or, l, r) :: tl ->
    simplify (exp_or l r tl) var value
  | _ -> clauses *)
(* the above is akin to variable expansion in a shell, what we rather want is
   reducing the amount of clauses directly. if a value is true, clause is removed *)
(* let rec simplify clauses var value =
  match clauses with
  | variable when variable = var && value -> *)


let rec get_unit_clauses clauses acc =
  match clauses with
  | [] -> acc
  | Var v :: tl -> get_unit_clauses tl (Var v :: acc)
  | UnaryOperator (_, v) :: tl -> get_unit_clauses tl (UnaryOperator(Not, v) :: acc)
  | _ -> get_unit_clauses clauses acc

let rec delete_k k acc =
  match acc with
  | [] -> []
  | (k', _) :: tl when k' = k -> delete_k k tl
  | hd :: tl -> hd :: delete_k k tl

let rec assoc_look k lst =
  match lst with
  | [] -> None
  | (k', v) :: tl -> 
    if k = k' then Some v
    else assoc_look k tl

let assign k v lst = (k, v) :: lst

let update_assign assignment units =
  List.fold_left (fun acc unit ->
    match unit with
    | Var v -> (v, true) :: delete_k v assignment
    | UnaryOperator(_, Var v) -> (v, false) :: delete_k v assignment
    | _ -> acc
  ) assignment units
(* 
let rec dpll clauses assignment =
  if contains_unsat clauses then UNSAT
  else
  if all_sat clauses then SAT
  else
  let units = get_unit_clauses clauses [] in
  let acc2 = update_assign assignment units in
  (* simplify *)
  let literals = get_literals clauses *)
  

  (* match clauses with
  | _ :: _ when contains_unsat clauses -> UNSAT
  | _ :: _ when all_sat clauses -> SAT *)

  (* splitting *)
  (* if contains_unsat clauses then UNSAT
  elif all_sat clauses then SAT *)

(* let sat formula =
  let tree = nnf_to_cnf @@ ast_to_nnf @@ str_to_ast formula in
  let clauses = cnf_to_clauses tree in
  let variables = get_literals tree in
  dpll clauses variables *)
(* 
    let pure_literal_elim literals clauses =
      List.map (fun clause ->
        match clause with
        | Operator (Or, Var v, other) ->
          if List.exists (fun y ->
            match y with
            | UnaryOperator (Not, Var w) -> v = w
            | _ -> false
          ) literals then [clause] else [other]
        | Operator (Or, UnaryOperator (Not, Var v), other) ->
          if List.exists (fun y ->
            match y with
            | Var w -> v = w
            | _ -> false
          ) literals then [clause] else [other]
        | Var v | UnaryOperator (Not, Var v) ->
          if List.exists (fun y -> y = Var v) literals then [] else [clause]
        | _ -> [clause]
      ) clauses *)