let fs = require("node:fs");

fs.readFile("./hello.in", "utf8", (err, data) => console.log(data));
