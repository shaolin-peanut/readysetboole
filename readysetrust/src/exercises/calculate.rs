pub fn adder(a: u32, b: u32) -> u32 {
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

pub fn multiplier(a: u32, b: u32) -> u32 {
    if b == 0 { return 0; }
    let mut sum = a;
    for _ in 1..b {
        sum = adder(sum, a);
    }
    sum
}

pub fn gray(a: u32) -> u32 {
    let delay = a >> 1;
    a ^ delay
}