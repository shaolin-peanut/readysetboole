open Ast
(* open Nnf *)
(* open Cnf *)
(* open Truthtable *)

(* seems unreadable but basically
   if the variable is positive, I check if there's a negative counterpart
   in the variables list
   if the it's negative, I check if there's a positive counterpart
  if there is one of those, it's a pure variable and I should return the other operand of OR
    and remove the pure literal *)

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
  ) clauses

let rec get_literals ast =
  match ast with
  | Var v -> [Var v]
  | UnaryOperator (_, child) -> [UnaryOperator (Not, child)]
  | Operator (_, left, right) ->
    let l = get_literals left in
    let r = get_literals right in
    List.sort_uniq compare (l @ r)
    | _ -> []

let cnf_to_clauses tree =
  match tree with
  | Operator (And, left, right) -> [left; right]
  | _ -> [tree]

let rec propagate v clauses =
  match clauses with
  | [] -> []
  | clause :: rest ->
    match clause with
    | Operator (Or, UnaryOperator (Not, v), other) -> [other] :: propagate v rest
      | Operator (Or, Var v, _) -> propagate (Var v) rest
      | _ -> [clause] :: propagate v rest

let rec unit_propagation clauses =
  match clauses with
  | [] -> []
  | clause :: rest ->
    match clause with
    | Var v -> [clause] :: propagate (Var v) rest
    | UnaryOperator (_, _)-> [clause] :: propagate clause rest
    | _ -> [clause] :: unit_propagation rest

let simplify clauses variables =
  let propagated = unit_propagation clauses in
  pure_literal_elim variables propagated

(* let rec dpll clauses variables =
  match clauses with
  (* empty close, unsat *)
  | [] -> Error "unsat"
  | _ ->
    match variables with
    | [] -> Error "sat"
    | _ ->
      let new_clauses, new_variables =
  simplify clauses variables *)

  (* if tree is empty, return true *)


(* let sat formula =
  let tree = nnf_to_cnf @@ ast_to_nnf @@ str_to_ast formula in
  let clauses = cnf_to_clauses tree in
  let variables = get_literals tree in
  dpll clauses variables *)
