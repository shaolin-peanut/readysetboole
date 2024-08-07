use crate::exercises::{Node, Operator};
use crate::exercises::evaluation::{evaluate, str_to_tree};

fn lookup_key(perm: &Vec<(char, bool)>, key: char) -> bool {
    for (k, v) in perm.iter() {
        if k == &key {
            return *v;
        }
    }
    panic!("Invalid key");
}


fn expand_ast(ast: &Node, perm: &Vec<(char, bool)>) -> Node {
    match ast {
        Node::Variable(v) => Node::Bool(lookup_key(perm, *v)),
        Node::Negation(child) => Node::Negation(Box::new(expand_ast(child, perm))),
        Node::BinaryOp { op, left, right } => {
            Node::BinaryOp {
                op: match op {
                    Operator::Not => Operator::Not,
                    Operator::And => Operator::And,
                    Operator::Or => Operator::Or,
                    Operator::Xor => Operator::Xor,
                    Operator::Cond => Operator::Cond,
                    Operator::Equal => Operator::Equal,
                },
                left: Box::new(expand_ast(left, perm)),
                right: Box::new(expand_ast(right, perm)),
            }
        },
        _ => panic!("Invalid ast"),
    }
}

fn generate_permutations_recursive(permutations: &mut Vec<Vec<(char, bool)>>, current_perm: &mut Vec<(char, bool)>, index: usize, vars: &Vec<char>) {
    if index == vars.len() {
        permutations.push(current_perm.clone());
        return;
    }
    
    current_perm.push((vars[index], false));
    generate_permutations_recursive(permutations, current_perm, index + 1, vars);
    current_perm.push((vars[index], true));
    generate_permutations_recursive(permutations, current_perm, index + 1, vars);
}

fn generate_permutations(vars: &Vec<char>) -> Vec<Vec<(char, bool)>> {
    let mut permutations: Vec<Vec<(char, bool)>> = Vec::new();
    let mut current_perm = vars.iter().map(
        |v| (*v, false)
    ).collect();
    
    generate_permutations_recursive(&mut permutations, &mut current_perm,0, vars);
    permutations
}

fn get_variables(ast: &Node) -> Vec<char> {
    match ast {
        Node::Variable(v) => vec![*v],
        Node::Negation(child) => get_variables(child),
        Node::BinaryOp { left, right, .. } => {
            let mut all = get_variables(left);
            all.extend(get_variables(right));
            all
        },
        _ => vec![],
    }
}

fn printing(ast: Node,  permutations: Vec<Vec<(char, bool)>>, vars: Vec<char>) {
    // header
    for var in vars.iter() { print!("{}  |", var);}
    println!(" output");
    // body
    for perm in permutations.iter() {
        for (_, v) in perm.iter() {
            print!("{}  |", v);
        }
        let ast = expand_ast(&ast, perm);
        println!("  {}", evaluate(&ast));
    }
}

// permutations look like this, it's an array of array of tuples
// I could have used a hashmap but this is simpler
// [(a, false), (b, true), (c, false)]
// [(a, false), (b, false), (c, true)]
// [(a, true), (b, false), (c, false)]
// [(a, true), (b, true), (c, false)]

pub fn print_truth_table(formula: &str) {
    let ast = str_to_tree(formula.to_string());
    let vars = get_variables(&ast);
    println!("{:?}", vars);
    let permutations: Vec<Vec<(char, bool)>> = generate_permutations(&vars);

    printing(ast, permutations, vars);
}
