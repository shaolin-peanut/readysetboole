open Ast

let neg node = UnaryOperator (Not, node)

let rec double_negation node =
  match node with
  | UnaryOperator (Not, UnaryOperator (Not, child)) -> double_negation child
  | UnaryOperator (Not, Boolean b) when b = false -> Boolean true
  | UnaryOperator (Not, child) -> UnaryOperator (Not, double_negation child)
  | Operator (op, l, r) -> Operator (op, double_negation l, double_negation r)
  | _ -> node

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

let ast_to_nnf tree =
  fixpoint double_negation tree
  |> fixpoint elim_equiv_cond
  |> fixpoint de_morgan

let nnf formula =
  Printf.printf "%s -> " formula;
  str_to_tree formula
  |> ast_to_nnf
  |> ast_to_rpn
  |> print_endline

