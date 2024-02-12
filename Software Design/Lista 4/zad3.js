var Person = function(name, surname) {
  this.name = name;
  this.surname = surname;
}
Person.prototype.say = function() {
  console.log(`${this.name} ${this.surname}`);
}
var Worker = function(name, surname, age) {
  Person.call(this, name, surname);
  this.age = age;
}

// this is wrong because adding methods to Worker.prototype adds them to 
// Person.prototype as well (which is obviously not intended)
//      Person {name, surname} 
//                            \
//                             -> (Person/Worker).prototype {say}
//                            /
// Worker {name, surname, age}
Worker.prototype = Person.prototype 
Worker.prototype.test = function() {};
console.log(Object.keys(Person.prototype));

// this causes the duplication of all properties of Person in the prototype chain, namely the name and 
// surname properties (waste of space)
//                                          Person {name, surname}
//                                                                \
// Worker {name, surname, age} -> Worker.prototype {name, surname} -> Person.prototype {say}  
Worker.prototype = new Person();
console.log(Object.keys(Worker.prototype));

// this is ideal because we don't duplicate any properties and we get access to all methods 
// defined for instances of Person and we can define methods usable only by Workers 
//                             Person {name, surname}
//                                                   \ 
// Worker {name, surname, age} -> Worker.prototype {} -> Person.prototype {say}
Worker.prototype = Object.create(Person.prototype);
console.log(Object.keys(Worker.prototype));
