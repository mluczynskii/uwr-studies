function for_each(xs, f) {
  if (xs.length <= 0) return;
  let [x, ...rest] = xs;
  _ = f(x);
  for_each(rest, f);
}
for_each([1,2,3], console.log);

function map(xs, f) {
  let ys = [];
  for_each(xs, x => ys.push(f(x)));
  return ys;
}
console.log(map([1,2,3], x => x * 2));

/*
function map2(xs, f) {
  return (function aux(ys, acc) {
    if (ys.length <= 0) return acc;
    let [y, ...rest] = ys;
    acc.push(f(y));
    return aux(rest, acc);
  })(xs, []); 
}
console.log(map2([1,2,3], x => x * 2));*/

function filter(xs, f) {
  let ys = [];
  for_each(xs, x => f(x) ? ys.push(x) : {});
  return ys;
}
console.log(filter([1,2,3,4], x => x % 2));
