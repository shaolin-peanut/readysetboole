use crate::exercises::{Node, Operator};
use std::collections::HashSet;

fn generate_permutations(ast: &Node, vars: Vec<char>) -> Vec<HashMap<char, bool>> {
    let mut permutations = Vec::new();
    let mut current_perm = vars.iter().map(|v| (*v, false)).collect();
    
    generate_permutations_recursive(
        &mut permutations,
        &mut current_perm,
        0,
        num_vars
    );
    
    permutations
}

// table of alphabetic variable to permutation index matching
// e.g. a -> 0, b -> 1, c -> 2
// this is used to turn the permutation into an ast

fn expand_ast(ast: &Node, permutation: &HashMap<char, bool>) -> Node {
    match ast {
        Node::Variable(v) => Node::Bool(permutation[v]),
        Node::UnaryOp { child, .. } => Node::UnaryOp {
            child: Box::new(expand_ast(child, permutation)),
            ..ast.clone()
        },
        Node::BinaryOp { left, right, .. } => {
            Node::BinaryOp {
                left: Box::new(expand_ast(left, permutation)),
                right: Box::new(expand_ast(right, permutation)),
                ..ast.clone()
            }
        },
        _ => panic!("Invalid ast"),
    }
}

fn generate_permutations_recursive(permutations: &mut Vec<HashMap<char, bool>>, current_perm: &mut HashMap<char, bool>, index: usize, num_vars: usize) {
    if index == num_vars {
        permutations.push(current_perm.clone());
        return;
    }

    // Try both true and false for the current variable
    current_perm.insert(vars[index], false);
    generate_permutations_recursive(permutations, current_perm, index + 1, num_vars);
    current_perm.insert(vars[index], true);
    generate_permutations_recursive(permutations, current_perm, index + 1, num_vars);
)

// fn generate_permutations_recursive(
//     permutations: &mut Vec<Vec<bool>>,
//     current_perm: &mut Vec<bool>,
//     index: usize,
//     num_vars: usize,
// ) {
//     if index == num_vars {
//         // as we have all permutations, we should turn this into an ast with variables replaced
//         permutations.push(current_perm.clone());
//         return;
//     }
    
//     // Try both true and false for the current variable
//     current_perm[index] = false;
//     generate_permutations_recursive(permutations, current_perm, index + 1, num_vars);
//     current_perm[index] = true;
//     generate_permutations_recursive(permutations, current_perm, index + 1, num_vars);
// }

fn main() {
    let permutations = generate_permutations(3);
    for perm in permutations {
        println!("{:?}", perm);
    }
}

fn generate_permutations(n: usize) -> Vec<Vec<bool>> {
    match n {
        0 => vec![vec![]],
        _ => {
            let tail = generate_permutations(n - 1);
            tail.into_iter().map(|l| vec![true, false]).chain(tail).collect()
        }
    }
}

fn get_variables(ast: &Node) -> Vec<char> {
    match ast {
        Node::Variable(v) => vec![*v],
        Node::UnaryOp { child, .. } => get_variables(child),
        Node::BinaryOp { left, right, .. } => {
            let mut all = get_variables(left);
            all.extend(get_variables(right));
            all
        },
        _ => vec![],
    }
}
pub fn print_truth_table(formula: &str) {
    let ast = str_to_tree(formula.to_string());
    let vars: Vec<char> = get_variables(&ast);
    let permutations = generate_permutations(&ast,vars);
}
