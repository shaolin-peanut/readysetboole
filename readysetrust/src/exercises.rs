pub mod calculate;
pub mod evaluation;
pub mod utils;

#[derive(Debug)]
pub enum Operator {
    Not,
    And,
    Or,
    Xor,
    Cond,
    Equal,
}

#[derive(Debug)]
pub enum Node {
    Bool(bool),
    Variable(char),
    UnaryOp {
        op: Operator,
        child: Box<Node>,
    },
    BinaryOp {
        op: Operator,
        left: Box<Node>,
        right: Box<Node>
    },
}

use std::fmt;

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op_str = match self {
            Operator::Not => "NOT",
            Operator::And => "AND",
            Operator::Or => "OR",
            Operator::Xor => "XOR",
            Operator::Cond => "=>",
            Operator::Equal => "==",
        };
        write!(f, "{}", op_str)
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Node::Bool(value) => write!(f, "{}", value),
            Node::Variable(var) => write!(f, "{}", var),
            Node::UnaryOp { op, child } => write!(f, "({} {})", op, child),
            Node::BinaryOp { op, left, right } => write!(f, "({} {} {})", left, op, right),
        }
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn it_works() {
//         let result = add(2, 2);
//         assert_eq!(result, 4);
//     }
// }
