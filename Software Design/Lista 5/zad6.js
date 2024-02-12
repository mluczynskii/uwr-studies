const fs = require("node:fs");
const readline = require("node:readline");

(async function read(filename) {
  const filestream = fs.createReadStream(filename);
  const rl = readline.createInterface({input: filestream});
  let dict = {};
  for await (const line of rl) {
    [timestamp, ip, request, resource, response] = line.split(' ');
    if (dict.hasOwnProperty(ip)) dict[ip] = dict[ip] + 1;
    else dict[ip] = 1;
  }
  results = Object.entries(dict).sort( ([,a], [,b]) => b-a).slice(0, 3);
  for (const [ip, count] of results) {
    console.log(`IP: ${ip} -> number of requests: ${count}`);
  }
  rl.close();
})('server.log');
