//SOLUTION
//FORBIDDEN sort\s*\(
//Don't sort the array
//IN 1 4 5 3 2
/**
   Find the second smallest element of a without sorting the array
   You may assume that a has at least two elements, and that
   all elements are integers.
*/
function secondSmallest(a) {
    //HIDE
    smallest = Math.min(a[0], a[1])
    secondSmallest = Math.max(a[0], a[1])
    for (i = 2; i < a.length; i++) {
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

a = readLine().split(/\s+/).map(function(n) { return parseInt(n) })
print("Second smallest: " + secondSmallest(a))
