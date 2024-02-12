// Mateusz Łuczyński 331826 L1.Z2
function digit_fold(n, f, acc) {
    let xs = n.toString();
    for (const c of xs) acc = f(acc, ~~c); // ~~ == cast char to int
    return acc;
} 

function pred(n) {
    let xs = digit_fold(n, (acc, k) => acc.concat([k]), []); // Extracts digits to a list
    let sum = digit_fold(n, (acc, k) => acc + k, 0); // Returns the sum of digits
    let flag = true;
    for (const k of xs) {
        if (k == 0) return false;
        flag &&= !(n % k);
    }
    return flag && !(n % sum);
}

function sieve(xs) {
    xs[0] = false;
    for (let i = 1; i < xs.length; i++) xs[i] = pred(i);
    return xs;
}

function weird_numbers(n) {
    let xs = sieve(Array.from({length : (n+1)}, _ => true));
    let ys = [];
    for (let i = 1; i <= n; i++) {
        if(xs[i]) ys.push(i);
    }
    return ys;
}

console.log(weird_numbers(100000));
