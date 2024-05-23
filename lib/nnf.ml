open Ast

let is_double_neg node =
  match node with
  | UnaryOperator (_, c) ->
    (fun node ->
      match node with
      | UnaryOperator (_, _) -> true
      | _ -> false) c
  | _ -> false

let get_op_char op =
  match op with
  | And -> "&"
  | Or -> "|"
  | Not -> "!"
  | Xor -> "^"
  | Cond -> ">"
  | Equiv -> "="

let rec ast_to_rpn node =
  match node with
  | Operator (op, l, r) -> ast_to_rpn l ^ ast_to_rpn r ^ get_op_char op
  | UnaryOperator (_, c) -> ast_to_rpn c ^ "!"
  | Var v -> String.make 1 v
  | Boolean b -> if b then "1" else "0"
  | Error msg -> raise (Failure msg)

let () =
  let ast = str_to_tree "10&" in
  ast_to_rpn ast |> print_endline;