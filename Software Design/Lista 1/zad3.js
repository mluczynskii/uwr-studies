// Mateusz Łuczyński 331826 L1.Z3
function sieve(xs) {
    xs[0] = xs[1] = false;
    let n = xs.length;
    for (let i = 2; i <= Math.sqrt(n); i++) {
        let p = Math.pow(i, 2);
        while (p <= n) {
            xs[p] = false;
            p += i;
        }
    }
    return xs;
}

function primes(n) {
    let xs = sieve(Array.from({length : (n+1)}, _ => true));
    let ys = [];
    for (let i = 2; i <= n; i++) {
        if (xs[i]) ys.push(i);
    }
    return ys;
}

console.log(primes(100000));





