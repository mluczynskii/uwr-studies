let foo = {
  name: 'Krzychu',
  surname: 'Torpeda',
  get fullname() { return this.name + ' ' + this.surname },
  set fullname(xs) { [this.name, this.surname] = xs.split(' ') },
  sayhello() { console.log(`Hello, I'm ${this.fullname}.`) }
}

foo.sayhello()
foo.fullname = "Kacper Szyszka"
foo.sayhello()

Object.defineProperty(foo, 'age', {
  enumerable: true,
  value: 10
})
Object.defineProperty(foo, 'saybye', {
  enumerable: true,
  value: () => console.log('bye!')
})
Object.defineProperty(foo, 'nationality', {
  enumerable: true,
  get() { return this._nationality },
  set(s) { this._nationality = s }
})

foo.networth = '100k+'
foo.gibberish = () => console.log('gluurb')

console.log(foo.nationality)
foo.nationality = 'Polish'
console.log(foo.nationality)
foo.saybye()
foo.gibberish()

console.log(foo)
