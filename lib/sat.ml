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

(* let pure_literal_elim literals clauses =
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
    | _ -> [] *)

let rec cnf_to_clauses tree =
  match tree with
  | Operator (And, left, right) :: tl -> (left, right) :: cnf_to_clauses tl 
  | _ -> []

let rec propagate v clauses =
  match clauses with
  | [] -> []
  | clause :: rest ->
    match clause with
      | Operator (Or, UnaryOperator (Not, v), other) -> [other] :: propagate v rest
      | Operator (Or, Var v, _) -> propagate (Var v) rest
      | _ -> [clause] :: propagate v rest

(* let rec unit_propagation clauses units =
  match clauses with
  | [] -> []
  | clause :: rest ->
    match clause with
    | Var v -> [clause] :: propagate (Var v) clauses
    | UnaryOperator (_, _)-> [clause] :: propagate clause clauses
    | _ -> [clause] :: unit_propagation rest *)
(* 
let rec get_unit_clauses clauses acc =
  match clauses with
  | [] -> acc
  | clause :: rest ->
    match clause with
    | Var v ->  get_unit_clauses rest ([Var v] :: acc)
    | UnaryOperator (_, v) -> get_unit_clauses rest ([UnaryOperator(Not, v)] :: acc)
    | _ -> get_unit_clauses clauses acc *)

(* let rec delete_k (k : char) (acc : (char * bool) list) : (char * bool) list = *)
let rec delete_k k acc =
  match acc with
  | [] -> []
  | (k', _) :: tl when k' = k -> tl
  | hd :: tl -> hd :: delete_k k tl

let rec assoc_look (k : 'a) (lst : ('a * 'b) list) : 'b option =
  match lst with
  | [] -> None
  | (k', v) :: tl -> 
    if k = k' then Some v
    else assoc_look k tl

let assign (k : 'a) (v : 'b) (lst : ('a * 'b) list) : ('a * 'b) list =
  (k, v) :: lst

let rec update_assign (assignment : (char * bool) list) (units : node list)
  List.fold_left (fun acc unit ->
    match unit with
    | Var v -> (v, true) :: delete v assignment
    | UnaryOperator(_, Var v) -> (v, false) :: delete v assignment
    | _ -> acc
  ) assignment units
  (*  List.iter( fun unit ->
    let update = delete_k unit assignment in
    match unit with 
    | Var v -> (v, true)
    | UnaryOperator (_, Var v) -> (v, false)
  ) units *)


(* something's wrong here, need to to learn more about list manipulation *)
(* https://cs3110.github.io/textbook/chapters/hop/fold.html *)
(* let rec update_assign (assignment: (char * bool) list) (units : node list) =
  List.fold_left (fun acc unit ->
    let assignment' = delete_k unit acc in
    match unit with
    | Var v -> (v, true) :: assignment'
    | UnaryOperator(_, Var v) -> (v , false) :: assignment'
    | _ -> acc
  ) assignment units *)
(* 
let rec dpll clauses assignment =
  if contains_unsat clauses then UNSAT else
  if all_sat clauses then SAT else
  let units = get_unit_clauses clauses [] in
  let assignment2 = update_assign assignment units in
  let propagated = unit_propagation clauses units in *)
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
