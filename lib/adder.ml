let rec adder a b =
  match a, b with
  | a, 0l -> a
  | a, b ->
    let carry = Int32.logand a b in
    adder (Int32.logxor a b) (Int32.shift_left carry 1)
