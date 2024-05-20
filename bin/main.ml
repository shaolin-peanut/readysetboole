open Readysetboole.Adder
open Readysetboole.Mult
open Printf

let () = 
  let add_two_numbers = adder 42l 42l in
  let num_plus_zero = adder 42l 0l in
  let only_zero = adder 0l 0l in

  printf "00 === ADDER\n";
  printf "Sum of 42l and 42l: %ld\n" add_two_numbers;
  printf "Sum of 42l and 0l: %ld\n" num_plus_zero;
  printf "Sum of 0l and 0l: %ld\n" only_zero;
  printf "\n";

  let mult_two_nums = mult 5l 5l in
  let mult_a_one = mult 1l 5l in
  let mult_b_one = mult 5l 1l in
  let mult_a_zero = mult 0l 5l in
  let mult_b_zero = mult 5l 0l in
  let mult_all_zero = mult 0l 0l in
  printf "01 === MULTIPLIER\n";
  printf "Product of 5 * 5: %ld\n" mult_two_nums;
  printf "Product of 5 * 0: %ld\n" mult_b_zero;
  printf "Product of 0 * 5: %ld\n" mult_a_zero;
  printf "Product of 0 * 0: %ld\n" mult_all_zero;
  printf "Product of 5 * 1: %ld\n" mult_b_one;
  printf "Product of 1 * 5: %ld\n" mult_a_one;

  