//SOLUTION
//FORBIDDEN sort\s*\(
//Don't sort the array
/**
   Find the second smallest element of a without sorting the array
   You may assume that a has at least two elements, and that
   all elements are integers.
*/
//CALL [1, 4, 5, 9, 2]
//CALL [9, 2, 3, 7, 5, 4]
//CALL [1, 1, 1, 1, 1, 1, 1]
function secondSmallest(a) {
    //HIDE
    smallest = Math.min(a[0], a[1])
    nextSmallest = Math.max(a[0], a[1])
    for (i = 2; i < a.length; i++) {
        if (a[i] < smallest) {
            nextSmallest = smallest;
            smallest = a[i];
        } else if (a[i] < nextSmallest) {
            nextSmallest = a[i];
        }
    }
    return nextSmallest;
    //SHOW ...
}

