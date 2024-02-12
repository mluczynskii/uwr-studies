function fib(n) {
  if (n <= 0) return 0;
  else if (n <= 2) return 1;
  return fib(n-1) + fib(n-2);
}

function memoize(f) {
  var cache = {};
  return function(n) {
    if (n in cache) return cache[n];
    else {
      let result = f(n);
      cache[n] = result;
      return result;
    }
  }
}

fib = memoize(fib);

/*var fib_mem2 = memoize( n => {
  if (n <= 0) return 0; 
  else if (n <= 2) return 1;
  return fib_mem2(n-1) + fib_mem2(n-2);
});*/

function fib_iter(n) {
  let [a, b] = [0, 1];
  for (let i = 2; i <= n; i++) [a, b] = [b, a+b];
  if (n <= 0) return a;
  return b;
}

var N = 25;
// pierwsze wywołanie fib_mem 
console.time(`fib_mem (first call), n = ${N}`);
_ = fib_mem(N);
console.timeEnd(`fib_mem (first call), n = ${N}`);
// drugie wywołanie fib_mem 
console.time(`fib_mem (second call), n = ${N}`);
_ = fib_mem(N);
console.timeEnd(`fib_mem (second call), n = ${N}`);
// fib_iter 
console.time(`fib_iter, n = ${N}`);
_ = fib_iter(N);
console.timeEnd(`fib_iter, n = ${N}`)

/*console.time('test');
_ = fib_mem2(N);
console.timeEnd('test');*/
