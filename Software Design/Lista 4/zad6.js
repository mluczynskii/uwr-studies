function Queue() {
  this.container = [];
}
Queue.prototype.push = function(item) {
  this.container.push(item);
}
Queue.prototype.pop = function() {
  if (this.container.length <= 0) return null;
  let [x, ...rest] = this.container;
  this.container = rest;
  return x;
}
Queue.prototype.isEmpty = function() {
  return this.container.length <= 0;
}

function Node(val, left=null, right=null) {
  this.left = left;
  this.val = val; 
  this.right = right;
}
Node.prototype[Symbol.iterator] = function*() {
  let q = new Queue();
  q.push(this);
  while (!q.isEmpty()) {
    let curr = q.pop();
    if (curr.left != null) q.push(curr.left);
    if (curr.right != null) q.push(curr.right);
    yield curr.val;
  }
}

//       root
//         1
//       /   \ 
//      2     4
//     /
//    3      

var root = new Node(1, new Node(2, new Node(3)), new Node(4));
for (var e of root) {
  console.log(e);
}

// expected -> 1 2 4 3
// with queue -> breadth-first search 
// with stack -> depth-first search 
