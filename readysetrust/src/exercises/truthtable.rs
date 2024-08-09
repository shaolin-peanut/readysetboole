use crate::exercises::Node;
use crate::exercises::evaluation::{evaluate, str_to_tree};

// TODO: rewrite all functions below without mutable structures
fn perm_get(perm: &Vec<(char, bool)>, key: char) -> bool {
    for (k, v) in perm.iter() {
        if k == &key {
            return *v;
        }
    }
    panic!("Invalid key");
}

fn perm_set(perm: &mut Vec<(char, bool)>, key: char, val: bool) {
    for (k, v) in perm.iter_mut() {
        if k == &key {
            *v = val;
            return;
        }
    }
    panic!("Invalid key");
}

fn expand_ast(ast: &Node, perm: &Vec<(char, bool)>) -> Node {
    match ast {
        Node::Variable(v) => Node::Bool(perm_get(perm, *v)),
        Node::Negation(child) => Node::Negation(Box::new(expand_ast(child, perm))),
        Node::BinaryOp { op, left, right } => {
            Node::BinaryOp {
                op: op.clone(),
                left: Box::new(expand_ast(left, perm)),
                right: Box::new(expand_ast(right, perm)),
            }
        },
        _ => panic!("Invalid ast"),
    }
}

fn generate_permutations_recursive(permutations: &mut Vec<Vec<(char, bool)>>, current_perm: &mut Vec<(char, bool)>, index: usize, vars: &Vec<char>) {
    if index == vars.len() {
        current_perm.reverse();
        return permutations.push(current_perm.clone())
    }
    let key = vars[index];
    
    perm_set(current_perm, key, false);
    generate_permutations_recursive(permutations, current_perm, index + 1, vars);
    perm_set(current_perm, key, true);
    generate_permutations_recursive(permutations, current_perm, index + 1, vars);
}

// implementation with mutable structures, bad
fn generate_permutations(vars: &Vec<char>) -> Vec<Vec<(char, bool)>> {
    let mut permutations: Vec<Vec<(char, bool)>> = Vec::new();
    let mut current_perm = vars.iter().map(
        |v: &char| (*v, false)
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
    print!("|");
    for var in vars.iter() { print!(" {} |", var);}
    println!(" = |\n|---|---|---|---|---|"); 
    // body
    for perm in permutations.iter() {
        print!("|");
        for (_, v) in perm.iter() {
            print!(" {} |", (if *v { "1" } else { "0" })); 
        }
        let ast = expand_ast(&ast, perm);
        println!(" {} |", (if evaluate(&ast) { "1" } else { "0" }));
    }
}

// TODO: verbose printing, with variable-expanded human-readeable formula in suffix notation,
// to check for correctness at a glance

// permutations look like this, it's an array of array of tuples
// I could have used a hashmap but this is simpler
// [(a, false), (b, true), (c, false)]
// [(a, false), (b, false), (c, true)]
// [(a, true), (b, false), (c, false)]
// [(a, true), (b, true), (c, false)]

pub fn print_truth_table(formula: &str) {
    println!("Truth table for {}", formula);
    let ast = str_to_tree(formula.to_string());
    println!("Ast form: {:?}", ast);
    let mut vars = get_variables(&ast);
    vars.reverse();
    let permutations: Vec<Vec<(char, bool)>> = generate_permutations(&vars);

    printing(ast, permutations, vars);
}
