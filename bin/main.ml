open Readysetboole.Adder
open Readysetboole.Mult
open Readysetboole.Gray
open Readysetboole.Ast
open Readysetboole.Truthtable
open Readysetboole.Nnf
open Readysetboole.Cnf
open Printf

let () = 
  
  printf "00 === ADDER\n";

  printf "Sum of 42l and 42l: %ld\n" @@ adder 42l 42l;
  printf "Sum of 42l and 0l: %ld\n" @@ adder 42l 0l;
  printf "Sum of 0l and 0l: %ld\n" @@ adder 0l 0l;
  printf "\n";

  printf "01 === MULTIPLIER\n";
  printf "Product of 5 * 5: %ld\n"  @@ mult 5l 5l;
  printf "Product of 5 * 0: %ld\n" @@ mult 5l 0l;
  printf "Product of 0 * 5: %ld\n" @@ mult 0l 5l;
  printf "Product of 0 * 0: %ld\n" @@ mult 0l 0l;
  printf "Product of 5 * 1: %ld\n" @@ mult 5l 1l;
  printf "Product of 1 * 5: %ld\n\n" @@ mult 1l 5l ;

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
  boolean_evaluator "110&|";
  boolean_evaluator "01&";
  boolean_evaluator "01&1|";

  printf("\ntests from subject.pdf\n");
  boolean_evaluator "10&";
  boolean_evaluator "10|";
  boolean_evaluator "11>";
  boolean_evaluator "10=";
  boolean_evaluator "1011||=";

  print_truth_table "BCA!&|";
  print_truth_table "BAC|!&";
  print_truth_table "XF&A|";
  (* test a logical expression in RPN with a lot more variables *)
  (* print_truth_table "ABCD&|X&Y&Z&|"; *)
  
  nnf "AB&!";
  nnf "AB|!";
  nnf "AB>";
  nnf "AB=";
  nnf "AB|C&!";

  print_endline "CNF tests:";
  cnf "AB&!";
  cnf "AB|!";
  cnf "AB|C&";
  (* cnf "AB|C|D";
  cnf "AB&C&D&";
  cnf "AB&!C!";
  cnf "AB|!C!&"; *)
  (* auto test *)
  (* cnf "AB|C&D";
  cnf "AB|C&D|E";
  cnf "AB|C&D|E&F"; *)


  