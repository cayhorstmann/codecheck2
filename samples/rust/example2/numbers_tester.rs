mod numbers;

fn main() {
    println!("{:?}", numbers::square(3));
    println!("Expected: 9");
    println!("{:?}", numbers::square(-3));
    println!("Expected: 9");
    println!("{:?}", numbers::square(0));
    println!("Expected: 0");
}