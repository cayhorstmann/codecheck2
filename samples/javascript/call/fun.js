//SOLUTION
//FORBIDDEN (for|while)\s*\(
//No loops!
/**
   Given an array of numbers, return the product of the square roots 
   of the non-negative ones. Use filter/map/reduce. Don't use a loop,
   and don't use recursion.
*/
//CALL [1, 4, -9, 16]
//CALL [1, 4, 9, 16]
//CALL [-1, -4, -9, -16]
//CALL [0, 1, 4, 9]
function prodNonNegRoots(a) {
    //HIDE
    return a.filter(function(x) { return x >= 0; }).map(function(x) { return Math.sqrt(x); } ).reduce(function(x, y) { return x * y }, 1);
    //SHOW return a....(...)....(...)....(...);
}
