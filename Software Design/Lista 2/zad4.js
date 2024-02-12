let s = "foo"
let p = new String("foo")

console.log(typeof s)
console.log(typeof p)

// typeof [expression]
// returns string representing the type of operand's value
// primitive types => {number, string, boolean, object, function, undefined}
// -> returns string

function Foo() {}
function Bar() {}
const foo = new Foo()

console.log(foo instanceof Foo)
console.log(foo instanceof Bar)
// [object] instanceof [constructor]
// instanceof checks if [object] was created using [contructor]
// -> returns boolean

const bar = new Bar()
console.log(typeof bar === typeof foo) 
// -> returns true despite the fact that their constructors
// have nothing to do with each other

console.log(s instanceof String)
// -> returns false because s is a primitive, not an object 
// created using String 
console.log(p instanceof String)
