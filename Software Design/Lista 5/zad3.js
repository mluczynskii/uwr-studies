const readline = require("node:readline");
const {stdin: input, stdout: output} = require("node:process");

let rl = readline.createInterface({input, output});

// rl.question wrapper
function question(q) {
  return new Promise( (res, rej) => {
    rl.question(q, ans => res(ans));
  })
}

(async function main() {
  let target = Math.floor(Math.random() * 100) // \in [0, 100]
  let guess;
  console.log("What number between 0-100 am I thinking of?")
  while (guess != target) {
    guess = await question("Your guess: ");
    if (guess > target) console.log("The number you've chosen is too big!");
    else if (guess < target) console.log("The number you've chosen is too small!");
  }
  console.log("You got it!");
  rl.close();
})();
