const express = require('express');
const http = require('node:http');

let app = express();

app.set('view engine', 'ejs');
app.set('views', './views');

app.use(express.static("./static"));

app.get('/', (req, res) => {
    let filename = "test.pdf";
    res.setHeader('Content-Disposition', `attachment; filename="${filename}"`);
    res.sendFile(filename, {root: './static/'});
});

http.createServer(app).listen(3000);