use crate::exercises::evaluation::str_to_tree;
use crate::exercises::{Node, Operator};

fn double_negation(node: &Node) -> Node {
    match node {
        Node::Negation(child) => double_negation(child),
        // Node::BinaryOp {..} => Node::map_children(node, double_negation),
        Node::BinaryOp { op, left, right } => Node::BinaryOp {
            op: op.clone(),
            left: Box::new(double_negation(left)),
            right: Box::new(double_negation(right)),
        },
        _ => node.clone(),
    }
}

// condition
// (A ⇒ B) ⇔ (¬A ∨ B)
// and equivalenve
// (A ⇔ B) ⇔ ((A ⇒ B) ∧ (B ⇒ A))
// so for equivalence we have to replace it by
// A ⇔ B ⇒ (¬A ∨ B) ∧ (A ⇒ ¬B)
// fn elim_equiv_cond(node: &Node) -> Node {
//     match node {
//         Node::BinaryOp { op: Operator::Cond, left, right } =>
//             Node::BinaryOp {
//                 op: Operator::Or,
//                 left: Box::new(Node::Negation (*left.clone())),
//                 right: Box::new(*right.clone()),
//             },
//         Node::BinaryOp { op: Operator::Equal, left, right } => Node::BinaryOp {
//             op: Operator::And,
//             left: Box::new(double_negation(left)),
//             right: Box::new(double_negation(right)),
//         },
//         _ => node.clone(),
//     }
// }

fn neg_rec(node: &Node) -> Node {
    let expanded_node = de_morgan(node);
    Node::Negation(Box::new(expanded_node))
}

// ¬(A ∨ B) ⇔ (¬A ∧ ¬B)
// ¬(A ∧ B) ⇔ (¬A ∨ ¬B)
fn de_morgan(node: &Node) -> Node {
    // anonymous bam function
    match node {
        Node::Negation(Node::BinaryOp{ op: Operator::Or, left, right }) =>
        {
                Node::BinaryOp {
                    op: Operator::And,
                    left: Box::new(neg_rec(left)),
                    right: Box::new(neg_rec(right)),
                }
        },
        Node::Negation(Node::BinaryOp{ op: Operator::And, left, right }) => {
            Node::BinaryOp {
                op: Operator::Or,
                left: Box::new(neg_rec(left)),
                right: Box::new(neg_rec(right)),
            }
        },
        Node::Negation(child) => Node::Negation(Box::new(de_morgan(child))),
        // replace last one with map_children when I get it working, 4 lines down to 1
        Node::BinaryOp { op, left, right } => Node::BinaryOp {
            op: op.clone(),
            left: Box::new(de_morgan(left)),
            right: Box::new(de_morgan(right)),
        },
        _ => node.clone(),
    }
}


pub fn nnf(formula: &str) -> String {
    let ast = str_to_tree(String::from(formula));
    // let nnf = ast_to_nnf(&ast);
    // ast_to_rpn(&nnf)
}