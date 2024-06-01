type node =
  | Operator of op * node * node
  | UnaryOperator of op * node
  | Boolean of bool
  | Var of char
  | Error of string

and op = And| Or | Not | Xor | Cond | Equiv
(* and op = '&' | '|' | ' *)

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

let is_alphabet c =
  match c with
  | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'V' | 'W' | 'X' | 'Y' | 'Z' -> true
  | _ -> false

let op_of_char = [
  ('&', And);
  ('|', Or);
  ('^', Xor);
  ('>', Cond);
  ('=', Equiv);
]

let str_to_tree formula = 
  let operands = Stack.create () in

  String.iter (fun token -> 
    match token with
    | '1' -> Stack.push (Boolean true) operands
    | '0' -> Stack.push (Boolean false) operands
    | '!' -> let child = Stack.pop operands in
        Stack.push (UnaryOperator (Not, child)) operands
    | c when List.mem_assoc c op_of_char ->
      let r, l = Stack.pop operands, Stack.pop operands in
      let node = Operator (List.assoc c op_of_char, r, l)
      in Stack.push node operands
    | c when is_alphabet c -> Stack.push (Var c) operands
    | _ -> raise (Failure "Invalid")
  ) formula;

  if Stack.length operands = 1 then
    Stack.pop operands
  else
    raise (Failure "Invalid formula")

let boolean_evaluator formula =
  str_to_tree formula
  |>  evaluate
  |> (fun x -> if x then "true" else "false")
  |> Printf.printf "%s -> %s\n" formula
