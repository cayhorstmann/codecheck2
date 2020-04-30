const average = require("./average.js").average;
console.log(average(3, 4));
console.log("Expected: 3.5");
console.log(average(3, 3));
console.log("Expected: 3");
console.log(average(0, -1));
console.log("Expected: -0.5");
