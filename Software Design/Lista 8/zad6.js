const express = require('express');
const http = require('node:http');
const csurf = require('csurf');
const cookieParser = require('cookie-parser');
const crypto = require('crypto');

var csrfProtection = csurf({ cookie: true })

let app = express();

app.set('view engine', 'ejs');
app.set('views', './views');

app.use(cookieParser())
app.use(express.urlencoded({extended:true})); 

app.get('/', csrfProtection, (req, res) => {
    res.render('csrf', {csrfToken: req.csrfToken()});
});
app.post('/', csrfProtection, (req, res) => {
    res.end(`Your favorite color is ${req.body.color}!`);
}); 

app.get('/rating', (req, res) => {
    res.render('web-tampering', {});
});
app.post('/rating', (req, res) => {
    let rating = req.body.rating;
    res.end(`Your rating of our service is ${rating} stars`);
});

const secret = 'gruncho goblin';

app.get('/article', (req, res) => {
    res.render('article', {});
});
app.post('/article', (req, res) => {
    let id = req.body.article_id;
    let hmac = crypto
                .createHmac('sha256', secret)
                .update(id)
                .digest('hex');
    let sign = encodeURIComponent(hmac);
    res.redirect(`/article/${id}?sign=${sign}`);
});
app.get('/article/:id', (req, res) => {
    if (!req.query.sign)
        res.end('Thief! (No HMAC sign)'); 
    let id = req.params.id;
    let hmac = crypto
                .createHmac('sha256', secret)
                .update(id)
                .digest('hex');
    if (encodeURIComponent(hmac) != req.query.sign) {
        res.end('Thief! (Wrong HMAC sign)')
    } else {
        res.end(`Your article ID is ${id}`);
    }
});

http.createServer(app).listen(3000);