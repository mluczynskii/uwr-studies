const express = require('express');
const cookieParser = require("cookie-parser");
const http = require('node:http');

let app = express();

app.set('view engine', 'ejs');
app.set('views', './views');

app.use(cookieParser());
app.use(express.urlencoded({extended: true}));
app.use(express.static("./static"));

app.get('/', (req, res) => {
    res.render('index', {
        name: '', 
        surname: '', 
        course: '', 
        message: ''
    });
});

app.post('/', (req, res) => {
    let { name, surname, course } = req.body;
    if (name && surname && course) {
        res.cookie("name", name).cookie("surname", surname).cookie("course", course);
        for(let idx = 1; idx <= 10; idx++) {
            let value = req.body[idx] ? 1 : 0;
            res.cookie(`${idx}`, value);
        }
        res.redirect('/declaration');
    } else {
        res.render('index', {
            name: name, 
            surname: surname, 
            course: course, 
            message: "Please provide your name, surname and course name" 
        });
    }
});

app.get('/declaration', (req, res) => {
    res.render('result', req.cookies);
});

http.createServer(app).listen(3000);