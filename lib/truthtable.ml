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
    List.map (fun l -> [true::l;false::l]) tail |> List.concat


let rec expand_values ast env =
  match ast with
  | Var v -> Boolean (List.assoc v env)
  | Boolean bool -> Boolean bool
  | UnaryOperator (op, child) ->
      let one = expand_values child env in
      UnaryOperator (op, one)
  | Operator (op, l, r) ->
      let right = expand_values l env
      and left = expand_values r env in
      Operator (op, left, right)
  | _ -> raise (Failure "expansion error")


let eval_and_print formula ast (maps: (char * bool) list list) =
  List.iter (fun (var, _) -> Printf.printf "| %c " var) (List.nth maps 0);
  Printf.printf "|\"%s\"\n" formula;
  Printf.printf "|%s|" @@ String.make ((List.length maps) * 2) '-' ;
  (* I'm using the reverse application operator here |>
     see it as a pipe in c where output becomes input of the next function
     The function commented-out above is the regular version of the same expression *)
  (* Printf.printf "|\"s\"\n" @@ List.length maps |> fun x -> x * 2 |> String.make x '-' |> *)
  Printf.printf "\n";

  List.iter (fun map ->
    List.iter (fun (_, value) -> Printf.printf "| %d " (Bool.to_int value)) map;
    let expanded_ast = expand_values ast map in
    let expr_result = evaluate expanded_ast in
    Printf.printf "| %d |\n" @@ Bool.to_int expr_result;
  ) maps;
  Printf.printf "|%s|\n"  @@ String.make ((List.length maps) * 2 ) '-'
  

let print_truth_table formula =
  let ast = str_to_tree formula in
  let vars = get_variables ast in
  let permutations = generate_permutations (List.length vars) in
  let maps = List.map (fun permutation -> List.combine vars permutation) permutations in
  eval_and_print formula ast maps;