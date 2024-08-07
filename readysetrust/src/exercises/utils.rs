use crate::exercises::evaluation::{str_to_tree, evaluate};
use crate::exercises::calculate::{adder, multiplier, gray};
use crate::exercises::Node;

pub fn print_binary(n: u32) -> String {
    (0..32)
        .rev()
        .map(|i| if (n & (1 << i)) != 0 { "1" } else { "0" })
        .collect()
}

pub fn test_numbers() {
    // test small and big, and print what you're testing
    println!("Testing {} + {}: {}", 0, 0, adder(0, 0));
    println!("Testing {} + {}: {}", 1, 0, adder(1, 0));
    println!("Testing {} + {}: {}", 0, 1, adder(0, 1));
    println!("Testing {} + {}: {}", 1, 1, adder(1, 1));
    println!("Testing {} + {}: {}", 2, 3, adder(2, 3));
    println!("Testing {} + {}: {}", 4, 7, adder(4, 7));
    println!("Testing {} + {}: {}", 5, 2, adder(5, 2));
    println!("Testing {} + {}: {}", 2, 19, adder(2, 19));
    println!("Testing {} * {}: {}", 0, 0, multiplier(0, 0));
    println!("Testing {} * {}: {}", 1, 0, multiplier(1, 0));
    println!("Testing {} * {}: {}", 0, 1, multiplier(0, 1));
    println!("Testing {} * {}: {}", 1, 1, multiplier(1, 1));
    println!("Testing {} * {}: {}", 2, 2, multiplier(2, 2));
    println!("Testing {} * {}: {}", 3, 3, multiplier(3, 3));
    println!("Testing {} * {}: {}", 4, 4, multiplier(4, 4));
    println!("Testing {} * {}: {}", 5, 5, multiplier(5, 5));
}

pub fn test_gray(n: u32) {
    let binary = print_binary(n);
    let gray_code = gray(n);
    let gray_binary = print_binary(gray_code);
    println!(
        "| {:10} | {:32} | {:32} | {:10} |",
        n, binary, gray_binary, gray_code
    );
}

pub fn test_evaluate() {
    let ast : Node = str_to_tree("10&".to_string());
    println!("10& -> {} = {}", ast, evaluate(&ast));

    let ast : Node = str_to_tree("10|".to_string());
    println!("10| -> {} = {}", ast, evaluate(&ast));

    let ast : Node = str_to_tree("11>".to_string());
    println!("11> -> {} = {}", ast, evaluate(&ast));

    let ast : Node = str_to_tree("10=".to_string());
    println!("10= -> {} = {}", ast, evaluate(&ast));

    let ast : Node = str_to_tree("1011||=".to_string());
    println!("1011|| -> {} = {}", ast, evaluate(&ast));

    let ast : Node = str_to_tree("10&".to_string());
    println!("10& -> {} = {}", ast, evaluate(&ast));

    let ast : Node = str_to_tree("101|&".to_string());
    println!("101|& -> {} = {}", ast, evaluate(&ast));
}