const express = require('express');
const http = require('node:http');

let app = express();

app.set('view engine', 'ejs');
app.set('views', './views');

app.get('/', (req, res) => {
    res.render('scam', {});
});

http.createServer(app).listen(1337);