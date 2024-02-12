var fib = function(n: number): number {
    if (n == 0) return 0;
    else if (n <= 2) return 1;
    return fib(n-1) + fib(n-2);
}

function memoize<T>(f: (n: number) => T): (n: number) => T {
    var cache: { [n: number]: T } = {};
    return function(n: number) {
        if (n in cache) return cache[n];
        else {
            let result = f(n);
            cache[n] = result;
            return result;
        }
    }
}

function test<T, K extends number | string | symbol>(f: (n: K) => T) {
    var cache: {[key: string | number | symbol]: T} = {}; // cheating
    return function(n: K) {
        if (n in cache) return cache[n];
        else {
            let result = f(n);
            cache[n] = result;
            return result;
        }
    }
} 

fib = memoize(fib);
console.log(fib(55));