open Ast

let is_double_neg = function
  | UnaryOperator (Not, UnaryOperator (Not, _)) -> true
  | _ -> false

let neg node = UnaryOperator (Not, node)

let rec elim_equiv_cond node =
  match node with
  | Operator (Equiv, l, r) ->
    let left = Operator (And, l, r) in
    let right = Operator (And, neg l, neg r) in
    Operator (Or, left, right)
  | Operator (Cond, l, r) ->
    Operator (Or, neg l, r)
  | UnaryOperator (Not, child) ->
    UnaryOperator (Not, elim_equiv_cond child)
  | Operator (op, l, r) ->
    Operator (op, elim_equiv_cond l, elim_equiv_cond r)
  | _ -> node

let rec de_morgan node =
  let bam x = neg @@ de_morgan x in
  (* this function recurs and negates  *)
  match node with
  | UnaryOperator (_, Operator (And, l, r)) ->
    Operator (Or, bam l, bam r)
  | UnaryOperator (_, Operator (Or, l, r)) ->
    Operator (And, bam l, bam r)
  | UnaryOperator (_, node) ->
    UnaryOperator (Not, de_morgan node)
  | Operator (op, l, r) ->
    Operator (op, de_morgan l, de_morgan r)
  | _ -> node

let char_of_op op =
  match op with
  | And -> "&"
  | Or -> "|"
  | Not -> "!"
  | Xor -> "^"
  | Cond -> ">"
  | Equiv -> "="
let rec ast_to_rpn node =
  match node with 
  | Operator (op, l, r) -> ast_to_rpn l ^ ast_to_rpn r ^ char_of_op op
  | UnaryOperator (_, c) -> ast_to_rpn c ^ "!"
  | Var v -> Printf.sprintf "%c" v
  | Boolean b -> if b then "1" else "0"
  | Error msg -> raise (Failure msg)

let rec fixpoint f node =
  let node' = f node in
  if node = node' then node else fixpoint f node'

let nnf formula =
  Printf.printf "%s -> " formula;
  str_to_tree formula
  |> fixpoint elim_equiv_cond
  |> fixpoint de_morgan
  |> ast_to_rpn
  |> print_endline


(* let () = *)
  (* let ast = str_to_tree "10&1|" in
  ast_to_rpn ast |> print_endline; *)