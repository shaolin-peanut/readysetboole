module IntSet = Set.Make(Int32)
module PowerSet = Set.Make(IntSet)

let get_singletons set =
  IntSet.map IntSet.singleton set

let powerset (set : IntSet.t) =
  let n = IntSet.cardinal set in
  Printf.printf "%d\n" n;
  let output = PowerSet.empty in
  PowerSet.add get_singletons output;
  PowerSet.to_list output