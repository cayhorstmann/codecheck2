//SOLUTION
use std::io;
fn main() {
    let mut done = false;
    while !done {
        println!("Enter a number, 0 to quit");

        let mut n = String::new();
        io::stdin().read_line(&mut n).expect("failed to readline");
        let mut trimmed = n.trim();
        let mut num = trimmed.parse::<i32>().unwrap();

        if num == 0 {
            done = true;
        } else {
            let mut square = num * num;
            println!("The square is {}", square);
        }
    }
}