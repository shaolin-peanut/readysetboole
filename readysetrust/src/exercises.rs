pub mod calculate;
pub mod evaluation;
pub mod utils;
pub mod truthtable;
pub mod nnf;

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
    Negation(Box<Node>),
    BinaryOp {
            op: Operator,
            left: Box<Node>,
            right: Box<Node>
        },
    // considering this, but lacking named field might
    // make logic less easy
    // BinaryOp(Operator, Box<Node>, Box<Node>),
}

impl Operator {
    pub fn new(op: Operator) -> Operator {
        op
    }
    pub fn clone(&self) -> Operator {
        match self {
            Operator::Not => Operator::Not,
            Operator::And => Operator::And,
            Operator::Or => Operator::Or,
            Operator::Xor => Operator::Xor,
            Operator::Cond => Operator::Cond,
            Operator::Equal => Operator::Equal,
        }
    }
}

impl Node {
    pub fn clone(&self) -> Node {
        match self {
            Node::Bool(value) => Node::Bool(*value),
            Node::Variable(var) => Node::Variable(*var),
            Node::Negation(child) => Node::Negation(Box::new(child.clone())),
            Node::BinaryOp { op, left, right } => Node::BinaryOp {
                op: op.clone(),
                left: Box::new((*left).clone()),
                right: Box::new((*right).clone()),
            },
        }
    }

    pub fn map_children(&self, f: fn(Node) -> Node) -> Node {
        match self {
            Node::BinaryOp { op, left, right } => Node::BinaryOp {
                op: op.clone(),
                left: Box::new(f((*left).clone())),
                right: Box::new(f((*right).clone())),
            },
            _ => self.clone(),
        }
    }
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
            Node::Negation(child) => write!(f, "!({})", child),
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
