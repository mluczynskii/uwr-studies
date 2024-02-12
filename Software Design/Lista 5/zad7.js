const fs = require("node:fs");
const util = require("node:util");

const filename = "hello.in";
const encoding = "utf8";

fs.readFile(filename, encoding, (err, data) => console.log(data)); // callback approach

// manual conversion + async/await
let _fsp = (function promisify(f) {
  return function(...args) {
    return new Promise( (res, rej) => {
      f(...args, (err, result) => {
        if (err) rej(err);
        else res(result);
      });
    });
  };
})(fs.readFile);

(async function() {
  let result = await _fsp(filename, encoding);
  console.log(result);
})()

// with util.promisify + .then
_fsp = util.promisify(fs.readFile);

_fsp(filename, encoding)
.then(data => console.log(data))
.catch(err => console.log(`err -> ${err}`));

// with fs.promises 
(async function() {
  let result = await fs.promises.readFile(filename, encoding);
  console.log(result);
})();



