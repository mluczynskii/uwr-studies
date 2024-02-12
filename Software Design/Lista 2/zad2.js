// chronicles of weird stuff in js 

// [] vs . operator on objects
let foo = {
  bar: 'helloworld',
  'foo bar': 'success',
}

console.log(foo.bar)
console.log(foo['bar'])

console.log(foo['foo bar']) // -> can only do that with []

let property = 'bar'
console.log(foo.property) // -> returns undefined
console.log(foo[property])

// [] operator on objects (2)
foo[1] = 'a number'
console.log(foo[1])

foo[{foo: 'bar'}] = 'an object'
console.log(foo[{foo: 'bar'}])

let bar = {
  toString() {return "using toString"}
}
foo[bar] = 'object with toString defined'

console.log(foo) // [object Object] vs toString

// [] operator on arrays
let xs = ['foo', 'bar']

console.log(xs['0']) // -> works perfectly fine
console.log(xs['a']) // -> returns undefined 

xs['a'] = 'foobar'
console.log(xs) 

xs[{foo: 'bar'}] = 'an object'
console.log(xs)

xs.length = 1
console.log(xs) // -> removes 'bar' but not a: 'foobar'...

xs.length = 5
console.log(xs) // -> <4 empty items>, 'bar' gone forever




