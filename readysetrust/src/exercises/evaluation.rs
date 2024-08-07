use crate::exercises::{Node, Operator};

pub fn str_to_tree(s: String) -> Node {
    let mut stack: Vec<Node> = Vec::new();

    for c in s.chars() {
        match c {
            'A'..='Z' => stack.push(Node::Variable(c)),
            '0' |'1' => stack.push(Node::Bool(c == '1')),
            '!' => {
                let child = Box::new(stack.pop().unwrap());
                stack.push(Node::UnaryOp {
                    op: Operator::Not,
                    child,
                });
            },
            '&' | '|' | '^' | '>' | '=' =>{
                let left = Box::new(stack.pop().unwrap());
                let right = Box::new(stack.pop().unwrap());
                stack.push(Node::BinaryOp {
                    op: match c {
                        '!' => Operator::Not,
                        '&' => Operator::And,
                        '|' => Operator::Or,
                        '^' => Operator::Xor,
                        '>' => Operator::Cond,
                        '=' => Operator::Equal,
                        _ => unreachable!(),
                    },
                    left,
                    right
                });
            },
            _ => unreachable!(),
        }
    }
    if stack.len() != 1 { panic!("Invalid formula"); }
    stack.pop().unwrap()
}

pub fn evaluate(node: &Node) -> bool {
    match node {
        Node::Bool(value) => *value,
        Node::Variable(var) => panic!("Variables can't be evaluated"),
        Node::UnaryOp { op, child } => {
            let child = evaluate(child);
            match op {
                Operator::Not => !child,
                _ => panic!("Invalid operator"),
            }
        },
        Node::BinaryOp { op, left, right } => {
            let left = evaluate(left);
            let right = evaluate(right);
            match op {
                Operator::And => left && right,
                Operator::Or => left || right,
                Operator::Xor => ((left && (!right)) || ((!left) && right) ),
                Operator::Cond => if left { right } else { false },
                Operator::Equal => left == right,
                _ => panic!("Invalid operator"),
            }
        }
    }
}

pub fn evaluate_formula(formula: &str) -> bool {
    let ast =str_to_tree(formula.to_string());
    evaluate(&ast)
}