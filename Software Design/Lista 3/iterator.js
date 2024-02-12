function createIterator(k) {
  return function() {
    var _state = 0;
    return {
      next: function() {
        return {
          value: _state,
          done: _state++ >= k
        };
      }
    };
  };
}

console.log('foo')
var foo = {
  [Symbol.iterator] : createIterator(5)
};
for (const f of foo) console.log(f);

console.log('bar')
var bar = {
  [Symbol.iterator] : createIterator(7)
};
for (const b of bar) console.log(b);
