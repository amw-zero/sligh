"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const sqlite3_1 = __importDefault(require("sqlite3"));
const index_1 = require("./index");
const db = new sqlite3_1.default.Database('./recur.db');
const app = (0, index_1.makeApp)(db);
(0, index_1.startApp)(app.app, () => { });
//# sourceMappingURL=serve.js.map