const express = require('express');
const http = require('node:http');
const multer  = require('multer')

const storage = multer.diskStorage({
    destination: function (req, file, cb) {
      cb(null, './static/uploads/');
    },
    filename: function (req, file, cb) {
      const uniqueSuffix = Date.now() + '-' + Math.round(Math.random() * 1E9) + ".jpg";
      cb(null, file.fieldname + '-' + uniqueSuffix);
    }
});
   
const upload = multer({storage: storage});

let app = express();

app.set('view engine', 'ejs');
app.set('views', './views');

app.use(express.urlencoded({extended:true}));
app.use(express.static("./static/uploads/"));

app.get('/', (req, res) => {
    res.render('zad1', {});
});

app.post('/', upload.single('meme'), (req, res) => {
    let img_name = req.file.filename;
    res.render('zad1', {filename: img_name});
});

http.createServer(app).listen(3000);