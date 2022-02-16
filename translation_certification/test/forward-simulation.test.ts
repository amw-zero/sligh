import 'mocha';
import fc from 'fast-check';
import { Database } from "sqlite3";
import { makeApp, startApp } from "../../web_server/dist/index";
import { transitionProperties } from "./state-transition-properties";

const db = new Database('../web_server/test.db');
const { app } = makeApp(db);
let server;

before((done) => {
  server = startApp(app, done);
});

after((done) => {
  server.close(done);
});

transitionProperties.forEach(({ name, property }) => {
  describe(`${name} - Forward Simulation`, function() {
    this.timeout(60000);

    it('simulates the model', async function() {
      await fc.assert(
        property.beforeEach(() => {
          return new Promise((resolve, reject) => {
            db.run("BEGIN TRANSACTION", (result, err) => {
              if (err) {
                reject(err);
              } else {
                resolve(true);
              }
            });
          });
        }).afterEach(() => {
          return new Promise((resolve, reject) => {
            db.run("ROLLBACK", (result, err) => {
              if (err) {
                reject(err);
              } else {
                resolve(true);
              }
            });
          });
        }),
        { numRuns: 5000 },
      );
    })
  });
});
