const fun = require('./fun.js')

console.log("Second smallest: " + fun.secondSmallest([1, 3, 5, 2, 4]))
console.log("Expected: " + 2)
console.log("Second smallest: " + fun.secondSmallest([1, 3, 5, 6, 4]))
console.log("Expected: " + 3)
