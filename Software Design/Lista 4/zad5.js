var Foo = function(name) {
  this.name = name;
}
Foo.prototype.Bar = function() {
  let Qux = () => console.log("Qux");
  Qux();
  console.log("Bar");
}

var foo = new Foo();
foo.Bar();
// foo.Qux(); -> throws exception
