// bugged example
var createFs = function(n) { 
  var fs = [];
  // i = 0;
  for (var i=0; i<n; i++) {
    fs[i] = function() { return i; };
  };
  return fs;
}

var myfs = createFs(10);
console.log(myfs[0]());
console.log(myfs[2]());
console.log(myfs[7]());
// -> they all point to the same variable i, which by the end 
// of the for loop has a value of 10

// fixed version
createFs = function(n) {
  var fs = [];
  for (var i = 0; i < n; i++) {
    fs[i] = (function() {
      var j = i; // -> this is only visible in this function and
                 // is not accessible after the function
                 // invokes itself (returned function will have 
                 // it in it's closure tho)
      return function() { return j; };
    })();
  }
  return fs;
}
myfs = createFs(10);
console.log(myfs[0]());
console.log(myfs[2]());
console.log(myfs[7]());

