open Adder

(* replace Int32.sub with your
    own function for substraction on signed ints *)

let mult a b =
  let rec aux a b res =
    match a, b with
    | _, 0l -> res
    | 0l, _ -> 0l
    | a, b -> aux a (Int32.sub b 1l) (adder res a)
  in aux a b 0l