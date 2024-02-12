// Mateusz Łuczyński 331826 L1.Z5
function fib_rec(n) {
    if (n == 0) return 0;
    else if (n == 1) return 1;
    return fib_rec(n-1) + fib_rec(n-2);
}

function fib_iter(n) {
    let a = 0, b = 1;
    for (let i = 2; i <= n; i++) {
        [a, b] = [b, a+b];
        /* let temp = a + b;
        a = b;
        b = temp; */
    }
    return (n == 0) ? a : b;
}

/* let n = 10
console.log(fib_rec(n));
console.log(fib_iter(n)); */

// note: 45 seems like a good enough ceiling without memory issues for fib_rec (>10s)
for (let i = 10; i < 45; i++) {
    let rec_label = `fib_rec n = ${i}`, iter_label = `fib_iter n = ${i}`;

    console.time(rec_label);
    _ = fib_rec(i);
    console.timeEnd(rec_label);

    console.time(iter_label);
    _ = fib_iter(i);
    console.timeEnd(iter_label);

    console.log("\n")
}