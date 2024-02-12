module.exports = {is_even};
let bar = require("./bar");

function is_even(n) {
  if (n == 0) return true;
  else return bar.is_odd(n-1);
}

