open Readysetboole.Adder
open Readysetboole.Mult
open Readysetboole.Gray
open Readysetboole.Ast
open Readysetboole.Truthtable
(* open Readysetboole.Astutils *)
open Printf

let () = 
  
  printf "00 === ADDER\n";
  let add_two_numbers = adder 42l 42l in
  let num_plus_zero = adder 42l 0l in
  let only_zero = adder 0l 0l in

  printf "Sum of 42l and 42l: %ld\n" add_two_numbers;
  printf "Sum of 42l and 0l: %ld\n" num_plus_zero;
  printf "Sum of 0l and 0l: %ld\n" only_zero;
  printf "\n";

  printf "01 === MULTIPLIER\n";
  let mult_two_nums = mult 5l 5l in
  let mult_a_one = mult 1l 5l in
  let mult_b_one = mult 5l 1l in
  let mult_a_zero = mult 0l 5l in
  let mult_b_zero = mult 5l 0l in
  let mult_all_zero = mult 0l 0l in
  printf "Product of 5 * 5: %ld\n" mult_two_nums;
  printf "Product of 5 * 0: %ld\n" mult_b_zero;
  printf "Product of 0 * 5: %ld\n" mult_a_zero;
  printf "Product of 0 * 0: %ld\n" mult_all_zero;
  printf "Product of 5 * 1: %ld\n" mult_b_one;
  printf "Product of 1 * 5: %ld\n\n" mult_a_one;

  printf "02 === GRAY CODES\n";
  printf "+-----+----------+----------+-----+\n";
  printf "| Dec  | Binary    | Gray Binary  | Gray Dec |\n";
  printf "+-----+----------+----------+-----+\n";
  test_gray 0l;
  test_gray 1l;
  test_gray 2l;
  test_gray 6l;
  test_gray 128l;
  test_gray 255l;
  printf "+-----+----------+----------+-----+\n";


  printf "\n03 == Boolean evaluation\n";
  printf "110&| -> %s\n" (boolean_evaluator (str_to_tree "110&|"));
  printf "01&   -> %s\n" (boolean_evaluator (str_to_tree "01&"));
  printf "01&1|  -> %s\n" (boolean_evaluator (str_to_tree "01&1|"));

  printf("\ntests from subject.pdf\n");
  printf "10& -> %s\n" (boolean_evaluator (str_to_tree "10&"));
  printf "10| -> %s\n" (boolean_evaluator (str_to_tree "10|"));
  printf "11> -> %s\n" (boolean_evaluator (str_to_tree "11>"));
  printf "10= -> %s\n" (boolean_evaluator (str_to_tree "10="));
  printf "1011||= -> %s\n" (boolean_evaluator (str_to_tree "1011||="));

  print_truth_table "BCA!&|";
  print_truth_table "BAC|!&";
  print_truth_table "XF&A|"

  