struct User {
    name: String,
    age: u32,
    height: u32,
}

fn print_user(user: User) {
    println!("Name: {}", user.name);
    println!("Age: {}", user.age);
    println!("Height: {}", user.height);
}

fn create_user(name: String, age: u32, height: u32) -> User {
    User {
        name,
        age,
        height,
    }
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

    let user = create_user(String::from("John"), 30, 170); 
    print_user(user);
}
