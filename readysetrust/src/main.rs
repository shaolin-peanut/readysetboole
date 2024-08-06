mod exercises;

use crate::exercises::utils::{test_numbers, test_gray, test_evaluate};
use crate::exercises::evaluation::evaluate_formula;

fn main() {
    test_numbers();
    println!("| {:10} | {:32} | {:32} | {:10} |", "decimal", "binary", "gray", "gray code");
    test_gray(0);
    test_gray(1);
    test_gray(2);
    test_gray(3);
    test_gray(4);
    test_gray(5);
    test_gray(6);
    test_gray(7);
    test_gray(8);
    test_gray(9);
    test_gray(11);
    test_gray(13);
    test_gray(15);
    test_gray(17);
    test_gray(19);
    test_gray(21);
    test_gray(23);
    test_gray(25);
    test_gray(27);
    test_gray(29);
    test_gray(31);

    // let ast = Node::BinaryOp {
    //     op: Operator::And,
    //     left: Box::new(Node::Variable('x')),
    //     right: Box::new(Node::UnaryOp {
    //         op: Operator::Not,
    //         child: Box::new(Node::Bool(true)),
    //     }),
    // };
    // test a bunch of complex boolean formulas, always input rpn without whitespaces
    // println!("{}", str_to_tree("10&".to_string()));
    // println!("{}", str_to_tree("10|".to_string()));
    // println!("{}", str_to_tree("10|1&".to_string()));
    // println!("{}", str_to_tree("101|&".to_string()));
    
    test_evaluate();
    // function like asked in the subject
    println!("10& -> {}", evaluate_formula("10&"));
}
