type node =
  | Operator of op * node * node
  | UnaryOperator of op * node
  | Boolean of bool
  | Var of char
  | Error of string

and op = And | Or | Not | Xor | Cond | Equiv


let rec evaluate node = 
  match node with
  | Boolean bool -> bool
  | Var _ -> raise (Failure "Variables can't be evaluated")
  | Operator (op, left_node, right_node) ->
    let eval_expr =
      let left = evaluate left_node in
      let right = evaluate right_node in
      match op with
      | And -> left && right
      | Or -> left || right
      | Xor -> ((left && (not right)) || ((not left) && right) )
      | Cond -> if left then right else false
      | Equiv -> left = right
      | _ -> false
    in eval_expr
  | UnaryOperator (_, term) -> not (evaluate term)
  | Error _ -> false
  
let boolean_evaluator node =
  if (evaluate node) then "true" else "false"

let make_node terms operator =
  match operator with
  | Not ->
    let term = Queue.take terms in
    UnaryOperator (operator, term)
  | And | Or | Xor | Cond | Equiv ->
    let right = Queue.take terms in
    let left = Queue.take terms in
    Operator (operator, right, left)

let is_alphabet c =
  match c with
  | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'V' | 'W' | 'X' | 'Y' | 'Z' -> true
  | _ -> false

let str_to_tree str =
  let operands = Queue.create () in
  let operators = Queue.create () in

  String.iter (fun token ->
    match token with
    | '1' -> Queue.add (Boolean true) operands
    | '0' -> Queue.add (Boolean false) operands
    | '!' -> Queue.add Not operators
    | '&' -> Queue.add And operators
    | '|' -> Queue.add Or operators
    | '^' -> Queue.add Xor operators
    | '>' -> Queue.add Cond operators
    | '=' -> Queue.add Equiv operators
    |  c ->
      if is_alphabet c
        then Queue.add (Var c) operands
      else raise (Failure "Invalid Character")
  ) str;

  while not (Queue.is_empty operators) do
    let op = Queue.take operators in
    let new_node = make_node operands op in
    Queue.add new_node operands;
  done;

  match Queue.take operands with
  | node -> node
  | exception Queue.Empty -> Error "Invalid expression"

  let rec print_node node =
    match node with
    | Operator (op, left, right) ->
      let op_str =
        match op with
        | And -> "&"
        | Or -> "|"
        | Xor -> "^"
        | Cond -> ">"
        | Equiv -> "="
        | _ -> ""
      in
      Printf.sprintf "(%s %s %s)" (print_node left) op_str (print_node right)
    | UnaryOperator (_, node) -> Printf.sprintf "(!%s)" (print_node node)
    | Boolean bool -> string_of_bool bool
    | Var c -> Printf.sprintf "%c" c
    | Error str -> str