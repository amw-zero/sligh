import express, { Express } from 'express';
import cors from "cors";
import { defineEndpoints } from "./define-endpoints";
import sqlite3 from "sqlite3"

sqlite3.verbose();

export function makeApp(db: sqlite3.Database) {
    const app = express();    
    
    app.use(cors({ origin: "*" }));
    app.use(express.json());
    defineEndpoints(app, db);

    return { app };
}

export function startApp(app: Express, onStart: () => void) {
    const port = 3000;
    return app.listen(port, () => {
        console.log(`Example app listening at http://localhost:${port}`)
        onStart();
    });
}

