const express = require('express');
const cors = require("cors");
const defineEndpoints = require("./define-endpoints");
const app = express();
const sqlite3 = require("sqlite3").verbose();

const db = new sqlite3.Database('./test.db');

const port = 3000;

app.use(cors({options: "*"}));

app.use(express.json());

app.post("/test", (req, res) => {
    console.log({body: req.body});
    res.send({});
});

defineEndpoints(app, db);

app.listen(port, () => {
console.log(`Example app listening at http://localhost:${port}`)
});
