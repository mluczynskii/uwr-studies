const http = require("node:https");

// standard interface
http.get('https://www.google.com', response => {
  var buf = '';
  response.on('data', data => buf += data.toString());
  response.on('end', () => console.log(buf));
});

// promise wrapper
function _getp(url) {
  return new Promise( (res, rej) => {
    http.get(url, response => {
      var buf = '';
      response.on('data', data => buf += data.toString());
      response.on('end', () => res(buf));
    });
  });
}
(async function() {
  let one = await _getp('https://stackoverflow.com/');
  console.log(one);
  let two = await _getp('https://www.youtube.com/');
  console.log(two);
})();
