use std::fmt;

pub enum Operator {
    Not,
    And,
    Or,
    Xor,
    Cond,
    Equal,
}


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

pub enum Status {
    SAT,
    UNSAT,
}

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

fn evaluate(node: Node) -> bool {
    match node {
        Node::Bool(value) => value,
        Node::Variable(var) => panic!("Variables can't be evaluated"),
        Node::UnaryOp { op, child } => {
            let child = evaluate(*child);
            match op {
                Operator::Not => !child,
                _ => panic!("Invalid operator"),
            }
        },
        Node::BinaryOp { op, left, right } => {
            let left = evaluate(*left);
            let right = evaluate(*right);
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

fn str_to_tree(s: String) -> Node {
    let mut stack: Vec<Node> = Vec::new();

    for c in s.chars() {
        match c {
            'a'..='z' => stack.push(Node::Variable(c)),
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
                        'c' => Operator::Cond,
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

fn print_binary(n: u32) -> String {
    (0..32)
        .rev()
        .map(|i| if (n & (1 << i)) != 0 { "1" } else { "0" })
        .collect()
}


fn adder(a: u32, b: u32) -> u32 {
    // b = 0 is the base case
    if b == 0 { return a; }
    // first we calculate if we have to carry a 
    // number to the next bit (in decimal like when you add two numbers whose sum is greater than 10)
    let carry = (a & b) << 1;
    // sum of the two bits, shifted to the left by one (does x2 in binary). carrying the carry
    let sum = a ^ b;
    // recursive call on sum plus carried bit
    adder(sum, carry)
}

// recursive implementation with forbidden - operator
// fn multiplier(a: u32, b: u32) -> u32 {
//     fn aux (a: u32, b: u32, res: u32) -> u32 {
//         if b == 0 {return res; }
//         else if a == 0 { return 0; }
//         else {
//             let new_res = adder(a, res);
//             aux(a, b - 1, new_res)
//         }
//     }
//     aux(a, b, 0)
// }

fn multiplier(a: u32, b: u32) -> u32 {
    if b == 0 { return 0; }
    let mut sum = a;
    for _ in 1..b {
        sum = adder(sum, a);
    }
    sum
}

fn gray(a: u32) -> u32 {
    let delay = a >> 1;
    a ^ delay
}

fn test_numbers() {
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

fn test_gray(n: u32) {
    let decimal = n;
    let binary = print_binary(n);
    let gray_code = gray(n);
    let gray_binary = print_binary(gray_code);
    println!(
        "| {:10} | {:32} | {:32} | {:10} |",
        decimal, binary, gray_binary, gray_code
    );
}

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
    println!("{}", str_to_tree("10&".to_string()));
    println!("{}", str_to_tree("10|".to_string()));
    println!("{}", str_to_tree("10|1&".to_string()));
    println!("{}", str_to_tree("101|&".to_string()));
    
}
