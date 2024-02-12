const https = require('node:https');
const fs = require('node:fs');

(async function main() {
    let pfx = await fs.promises.readFile('uwr.pfx');
    let server = https.createServer({
        pfx: pfx, 
        passphrase: 'uwr'
    }, (req, res) => {
        res.setHeader('Content-type', 'text/html; charset=utf-8');
        res.end('Hello world!');
    });
    server.listen(3000);
    console.log('running...');
})();