"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.startApp = exports.makeApp = void 0;
const express_1 = __importDefault(require("express"));
const cors_1 = __importDefault(require("cors"));
const define_endpoints_1 = require("./define-endpoints");
const sqlite3_1 = __importDefault(require("sqlite3"));
sqlite3_1.default.verbose();
function makeApp(db) {
    const app = (0, express_1.default)();
    app.use((0, cors_1.default)({ origin: "*" }));
    app.use(express_1.default.json());
    (0, define_endpoints_1.defineEndpoints)(app, db);
    return { app };
}
exports.makeApp = makeApp;
function startApp(app, onStart) {
    const port = 3000;
    return app.listen(port, () => {
        console.log(`Example app listening at http://localhost:${port}`);
        onStart();
    });
}
exports.startApp = startApp;
//# sourceMappingURL=index.js.map