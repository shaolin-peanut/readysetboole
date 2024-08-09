use crate::exercises::evaluation::str_to_tree;
use crate::exercises::{Node, Operator};

fn double_negation(node: Node) -> Node {
    match node {
        Node::Negation(child) => {
            match *child {
                Node::Negation(c) => double_negation(*c),
                _ => Node::Negation(Box::new(double_negation(*child))),
            }
        },
        Node::BinaryOp { .. } => 
            Node::map_children(&node, double_negation),
        _ => node.clone(),
    }
}

// condition
// (A ⇒ B) => (¬A ∨ B)
// and equivalenve
// (A ⇔ B) => ((A ⇒ B) ∧ (B ⇒ A))
// so for equivalence we have to replace it by
// A ⇔ B => (¬A ∨ B) ∧ (A ⇒ ¬B)
fn elim_equiv_cond(node: Node) -> Node {
    match node {
        Node::BinaryOp { op: Operator::Cond, left, right } =>
            Node::BinaryOp {
                op: Operator::Or,
                left: Box::new(Node::Negation(left)),
                right,
            },
        Node::BinaryOp { op: Operator::Equal, left, right } => Node::BinaryOp {
            op: Operator::And,
            left: Box::new(Node::BinaryOp {
                op: Operator::Or,
                left: Box::new(Node::Negation(left.clone())),
                right: right.clone(),
            }),
            right: Box::new(Node::BinaryOp {
                op: Operator::Or,
                left,
                right: Box::new(Node::Negation(right)),
            }),
        },
        _ => node.clone(),
    }
}

fn neg_rec(node: Node) -> Node {
    let expanded_node = de_morgan(node);
    Node::Negation(Box::new(expanded_node))
}

// ¬(A ∨ B) ⇔ (¬A ∧ ¬B)
// ¬(A ∧ B) ⇔ (¬A ∨ ¬B)
fn de_morgan(node: Node) -> Node {
    match node {
        Node::Negation(child) => {
            match *child {
                Node::BinaryOp { op: Operator::Or, left, right } => {
                    Node::BinaryOp {
                        op: Operator::And,
                        left: Box::new(neg_rec(*left)),
                        right: Box::new(neg_rec(*right)),
                    }
                },
                Node::BinaryOp { op: Operator::And, left, right } => {
                    Node::BinaryOp {
                        op: Operator::Or,
                        left: Box::new(neg_rec(*left)),
                        right: Box::new(neg_rec(*right)),
                    }
                },
                _ => Node::Negation(Box::new(de_morgan(*child))),
            }
        },
        _ => node,  // No need to clone if you own the value
    }
}

pub fn negation_normal_form(formula: &str) -> String {
    let ast = str_to_tree(String::from(formula));
    let res = de_morgan(double_negation(ast));
    //ast_to_rpn(&res);
    String::from("yo")
    // let nnf = ast_to_nnf(&ast);
    // ast_to_rpn(&nnf)
}

// tests

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_de_morgan() {
        let node = Node::Negation(Box::new(Node::BinaryOp { op: Operator::Or, left: Box::new(Node::Variable('A')), right: Box::new(Node::Variable('B')) }));
        let expected = Node::BinaryOp { op: Operator::And, left: Box::new(Node::Negation(Box::new(Node::Variable('A')))), right: Box::new(Node::Negation(Box::new(Node::Variable('B')))) };
        // assert_eq!(de_morgan(node), expecFAILted);
        let res = de_morgan(node);
        if res == expected {
            println!("OK");
        } else {
            // panic
            println!("Result: {:?}", res);
            panic!("FAILED");
        }
    }

    #[test]
    fn test_neg_rec() {
        let node = Node::BinaryOp {
            op: Operator::And,
            left: Box::new(Node::Negation(Box::new(Node::Variable('A')))),
            right: Box::new(Node::Negation(Box::new(Node::Variable('B')))),
        };
        let expected = Node::BinaryOp {
            op: Operator::Or,
            left: Box::new(Node::Negation(Box::new(Node::Variable('A')))),
            right: Box::new(Node::Negation(Box::new(Node::Variable('B')))),
        };
        assert_eq!(neg_rec(node), expected);
    }

}