//SOLUTION
//FORBIDDEN sort\s*\(
//Don't sort the array
/**
   Find the second smallest element of a without sorting the array
   You may assume that a has at least two elements, and that
   all elements are integers.
*/
function secondSmallest(a) {
    //HIDE
    let smallest = Math.min(a[0], a[1])
    let secondSmallest = Math.max(a[0], a[1])
    for (let i = 2; i < a.length; i++) {
        if (a[i] < smallest) {
            secondSmallest = smallest;
            smallest = a[i];
        } else if (a[i] < secondSmallest) {
            secondSmallest = a[i];
        }
    }
    return secondSmallest;
    //SHOW ...
}

const a = [1, 3, 5, 2, 4] //SUB [3, 4, 7, 11, 2] ; [3, 4] ; [3, 3, 3]

console.log("Second smallest: " + secondSmallest(a))
