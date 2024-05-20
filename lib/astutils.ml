(* type node =
  | Operator of op * node * node
  | UnaryOperator of op * node
  | Boolean of bool
  | Error of string

and op = And | Or | Not | Xor | Cond | Equiv

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
  | UnaryOperator (_, node) -> Printf.sprintf "(%s%s)" "!" (print_node node)
  | Boolean bool ->
    string_of_bool bool
  | Error str -> str *)