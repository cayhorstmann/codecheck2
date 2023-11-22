//SOLUTION
//CALL 5
//CALL 10
//CALL 1
pub fn create_array(x: i32) -> Vec<i32> {
    let mut arr: [i32; 5] = [0,0,0,0,0];
    for n in 0..arr.len() {
        arr[n] = n as i32 + x;
    }

    return arr.to_vec();
}