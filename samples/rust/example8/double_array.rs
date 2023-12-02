//SOLUTION
fn main() {
    let mut arr: [i32; 5]= [1,2,3,4,5]; //SUB [2,4,6,8,10]; [5,10,15,20,25]; [10,20,30,40,50]

    for n in 0..arr.len() {
        arr[n] = arr[n] * 2;
    }

    println!("{:?}", arr);
}