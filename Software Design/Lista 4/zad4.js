var n = 1;

console.log(typeof Object.getPrototypeOf(n)); // returns Object (actually the type of the wrapper object)

n.foo = 'foo';
console.log(n.foo); // returns undefined because the assignment takes place on a temporary wrapper object

/* from https://developer.mozilla.org/en-US/docs/Glossary/Primitive ->
[...] When properties are accessed on primitives, JavaScript auto-boxes the value into a wrapper object 
and accesses the property on that object instead. For example, "foo".includes("f") implicitly creates a String wrapper object 
and calls String.prototype.includes() on that object. [...] "mutating" primitives does not work,
because n.foo = 'foo' is not assigning to the property foo of n itself, but to an ephemeral wrapper object. */
