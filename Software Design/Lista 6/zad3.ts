function filter<T> (xs: T[], pred: (x: T) => boolean) : T[] {
    return (function aux(ys: T[], acc: T[]) {
        if (ys.length <= 0) return acc;
        let [y, ...rest] = ys;
        if (pred(y)) acc.push(y);
        return aux(rest, acc);
    })(xs, []);
}
let a = [1, 2, 3, 4];
console.log(filter(a, x => x % 2 == 0));
// console.log("ts-watch test");

function map<T, S> (xs: T[], f: (x: T) => S) : S[] {
    return (function aux(ys: T[], acc: S[]) {
        if (ys.length <= 0) return acc;
        let [y, ...rest] = ys;
        acc.push(f(y));
        return aux(rest, acc);
    })(xs, []);
}
let b = ['abc', 'b', 'cdefgh'];
console.log(map(b, s => s.length));

function forEach<T> (xs: T[], f: (x: T) => void) : void {
    if (xs.length <= 0) return;
    let [x, ...rest] = xs;
    f(x);
    forEach(rest, f);
}
let c = [true, false, true];
forEach(c, console.log);