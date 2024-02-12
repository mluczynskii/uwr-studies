function getLastProto(obj) {
  var p = obj;
  do {
    obj = p;
    p = Object.getPrototypeOf(obj);
  } while(p);
  return obj;
}

var foo = {};
var bar = {};
var foobar = {};

// foobar -> bar -> foo -(?)-> Object.prototype
Object.setPrototypeOf(foobar, bar);
Object.setPrototypeOf(bar, foo);

console.log(getLastProto(foo) === getLastProto(bar));
console.log(getLastProto(bar) === getLastProto(foobar));
console.log(getLastProto(foobar) === Object.prototype);
// returns true for all tests
