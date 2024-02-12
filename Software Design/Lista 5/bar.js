module.exports = {is_odd};
let foo = require("./foo");

function is_odd(n) {
  if (n == 0) return false;
  else return foo.is_even(n-1);
}
