const express = require('express');
const http = require('node:http');
const cookieParser = require('cookie-parser');

let app = express();

app.set('view engine', 'ejs');
app.set('views', './views');
app.disable('etag');

app.use(cookieParser());

//can check cookie support only client-side from navigator.cookieEnabled

app.get('/', (req, res) => {
    res.render('zad4', req.cookies);
});

app.post('/', (req, res) => {
    let cookie = req.cookies.cookie;
    if (!cookie) { // Cookie not found -> add one
        let options = {
            hour: "2-digit",
            minute: "2-digit",
            second: "2-digit",
            hour12: false
        }
        cookie = new Date().toLocaleDateString('pl-PL', options);
        res.cookie('cookie', cookie);
    } else { // Cookie found -> delete it
        res.cookie('cookie', 1, {maxAge: -1});
    }
    res.redirect('/');
});

http.createServer(app).listen(3000);