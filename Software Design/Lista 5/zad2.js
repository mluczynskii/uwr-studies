const readline = require('node:readline');
const { stdin: input, stdout: output} = require('node:process');

let rl = readline.createInterface({input, output});
rl.question("What's your name? ", ans => {
  console.log(`Hello, ${ans}!`);
  rl.close();
});
