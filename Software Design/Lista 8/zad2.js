const express = require('express');
const http = require('node:http');

let app = express();

app.set('view engine', 'ejs');
app.set('views', './views');

app.get('/', (req, res) => {
    let radio = {
        name: 'decision',
        options: [
            {label: 'Yes', value: 'confirm'},
            {label: 'No', value: 'decline'},
        ]
    }
    res.render('zad2', {radio});
});

http.createServer(app).listen(3000);