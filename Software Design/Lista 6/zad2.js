var fib = function (n) {
    if (n == 0)
        return 0;
    else if (n <= 2)
        return 1;
    return fib(n - 1) + fib(n - 2);
};
function memoize(f) {
    var cache = {};
    return function (n) {
        if (n in cache)
            return cache[n];
        else {
            var result = f(n);
            cache[n] = result;
            return result;
        }
    };
}
function test(f) {
    var cache = {}; // cheating
    return function (n) {
        if (n in cache)
            return cache[n];
        else {
            var result = f(n);
            cache[n] = result;
            return result;
        }
    };
}
fib = memoize(fib);
console.log(fib(55));
