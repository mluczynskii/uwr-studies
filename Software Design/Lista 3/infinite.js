function fib_it() {
  let [a, b] = [0, 1];
  return {
    next: function() {
      let result = {
        value: a,
        done: false
      };
      [a, b] = [b, a+b];
      return result;
    }
  };
}

var foo = {
  [Symbol.iterator]: fib_it
};
// for(const f of foo) console.log(f); // works fine

function* fib_gen() {
  let [a, b] = [0, 1];
  while(true) {
    yield a;
    [a, b] = [b, a+b];
  }
}

var bar = fib_gen();
// for (const b of bar) console.log(b); // works fine as well

function* take(it, N) {
  let cnt = 0;
  for (var _result; _result = it.next(), !_result.done; ) {
    if (cnt >= N) break;
    yield _result.value;
    cnt++;
  }
}

for (const num of take(fib_it(), 10)) console.log(num)
for (const num of take(fib_gen(), 10)) console.log(num)

