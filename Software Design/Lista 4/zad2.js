function directly(obj) { 
  return Object.getOwnPropertyNames(obj); 
}

function indirectly(obj) {
  let result = (function aux(p, acc) {
    if (p == null) return acc;
    acc = acc.concat(acc, directly(p));
    return aux(Object.getPrototypeOf(p), acc);
  })(obj, []);
  return removeDuplicates(result);
}

function removeDuplicates(xs) {
  return xs.reduce(function(acc, x) {
    acc = acc.filter(y => y != x);
    return acc.concat([x]);
  }, [])
}

function probe(obj, property) {
  let [xs, ys] = [directly(obj), indirectly(obj)];
  if (xs.includes(property)) console.log("Property comes directly from given object");
  else if (ys.includes(property)) console.log("Property comes from the prototype chain of given object");
  else console.log("Property is neither in the given object nor it's prototype chain");
}

var foo = { name: "Krzychu" }
var bar = { surname: "Bomba" }
Object.setPrototypeOf(bar, foo);

probe(bar, "surname");
probe(bar, "name");
probe(bar, "age");
