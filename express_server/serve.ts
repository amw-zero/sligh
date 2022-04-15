import sqlite3 from "sqlite3";
import { makeApp, startApp } from "./index";

const db = new sqlite3.Database('./recur.db');

const app = makeApp(db);

startApp(app.app, () => {});
