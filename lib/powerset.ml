module IntSet = Set.Make(Int32)
module PowerSet = Set.Make(IntSet)
let int_set_to_string set =
  let elements = IntSet.elements set in
  let elements_str = List.map Int32.to_string elements in
  "{" ^ (String.concat ", " elements_str) ^ "}"

(* Function to convert PowerSet to a string *)
let power_set_to_string power_set =
  let sets = PowerSet.elements power_set in
  let sets_str = List.map int_set_to_string sets in
  "{" ^ (String.concat "; " sets_str) ^ "}"

let rec get_subsets set =
  match set with 
  | set when IntSet.is_empty set -> PowerSet.singleton IntSet.empty
  | _ -> (* take the head, recurse over the tail to avoid duplication of subsets*)
    let head = IntSet.choose set in
    let tail = IntSet.remove head set in
    let subsets_without_hd = get_subsets tail in
    let subsets_with_hd = PowerSet.map (IntSet.add head) subsets_without_hd in
    PowerSet.union subsets_without_hd subsets_with_hd

let powerset (set : IntSet.t) =
  get_subsets set