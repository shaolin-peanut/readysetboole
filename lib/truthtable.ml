open Ast

let rec get_variables ast =
  match ast with
  | Var v -> [v]
  | UnaryOperator (_, child) -> get_variables child
  | Operator (_, left, right) ->
    let l = get_variables left in
    let r = get_variables right in
    List.sort_uniq Char.compare (l @ r)
    | _ -> []

let rec generate_permutations n =
  match n with
  | 0 -> [[]]
  | _ ->
    let tail = generate_permutations (n - 1) in
    List.concat (List.map (fun l -> [true::l;false::l]) tail)

let rec expand_values ast env =
  match ast with
  | Var v -> Boolean (List.assoc v env)
  | Boolean bool -> Boolean bool
  | UnaryOperator (op, child) ->
      let one = expand_values child env in
      UnaryOperator (op, one)
  | Operator (op, l, r) ->
      let right = expand_values l env in 
      let left = expand_values r env in
      Operator (op, left, right)
  | _ -> raise (Failure "expansion error")



let print_truth_table formula =
  let ast = str_to_tree formula in
  let vars = get_variables ast in
  let permutations = generate_permutations (List.length vars) in

  List.iter (Printf.printf "| %c " ) vars;
  Printf.printf " | = |\n";
  Printf.printf "|%s|" (String.make ((List.length vars) * 2 + 10) '-') ;
  Printf.printf "\n";

  List.iter (fun permutation ->
    print_string "|";
    let var_map = List.combine vars permutation in
    let expanded_ast = expand_values ast var_map in
    List.iter (fun b -> Printf.printf " %d  " (Bool.to_int b)) permutation;
    Printf.printf "| %d |\n" (Bool.to_int (evaluate expanded_ast))
  ) permutations;
  Printf.printf "|%s|\n" (String.make ((List.length vars) * 2 + 10) '-') ;