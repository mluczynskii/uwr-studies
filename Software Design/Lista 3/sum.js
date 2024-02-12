function sum(...args) {
  let acc = 0;
  for (const n of args) acc = acc + n;
  return acc;
}

console.log(sum(1,2,3));
console.log(sum(1,2,3,4,5));
