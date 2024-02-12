const express = require('express');
const http = require('node:http');
const session = require('express-session');
const FileStore = require('session-file-store')(session);

let app = express();

app.use(session({
    store: new FileStore,
    secret: 'gruncho goblin',
    resave: true,
    saveUninitialized: true
  })
);

app.get('/', (req, res) => {
    if (req.session.views) req.session.views++;
    else req.session.views = 1;
    res.end(`number of visits: ${req.session.views}`);
});

http.createServer(app).listen(3000);