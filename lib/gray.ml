
let gray a =
  let delay = Int32.shift_right a 1 in
  Int32.logxor a delay;
  
(* print utilies below, ignore *)

let print_binary (n: int32) =
  let rec aux i =
    if i < 0 then "" else
    let bit = (Int32.logand n (Int32.shift_left 1l i)) <> 0l in
    (if bit then "1" else "0") ^ aux (i - 1)
  in
  aux 31

let test_gray (n: int32) =
  let decimal = n in
  let binary = print_binary n in
  let gray_code = gray n in
  let gray_binary = print_binary gray_code in
  printf "| %10d | %32s | %32s | %10ld |\n" decimal binary gray_binary gray_code
  