import { expect } from 'chai';
import 'mocha';
import { Budget as Fullstack,  } from "./generated-client";
import { Budget as Model } from "./generated-model"
import fc from 'fast-check';
import { Database } from "sqlite3";
import { makeApp, startApp } from "../../web_server/dist/index";

const db = new Database('../web_server/test.db');
const { app } = makeApp(db);
let server;

before((done) => {
  server = startApp(app, done);
})

after((done) => {
  server.close(done);
})

describe('FullstackBudget', function() {
  this.timeout(60000);

  it('simulates the model', async function() {
    await fc.assert(
      // derive generators from schema defs
      fc.asyncProperty(fc.record({ name: fc.string(), amount: fc.float() }), async (crt) => {
        console.log("Test body");

        return new Promise(async (resolve, reject)=> {
          try {
            console.log("Begining transaction");
            db.exec("BEGIN TRANSACTION");
            let model = new Model();
            let fullstack = new Fullstack(() => {});
    
            // derive action name from state transition
            let createdRt = await fullstack.create_recurring_transaction(crt);
            model.create_recurring_transaction(crt, createdRt.id);
                
            expect(fullstack.recurring_transactions).to.deep.eq(model.recurring_transactions);
    
            // make sure to call read!s
            await fullstack.view_recurring_transactions();

            expect(fullstack.recurring_transactions).to.deep.eq(model.recurring_transactions);
          } catch(e) {
            console.log({error: e});
            reject(e);
          } finally {
            console.log("Rolling back");              
            db.exec("ROLLBACK", () => {
              console.log("Done rolling back")
              resolve(true);
            });
          }
        });
       
      })/*.beforeEach(() => {
        return new Promise((resolve, reject) => {
          db.run("BEGIN TRANSACTION", (result, err) => {
            console.log("Begin  transaction");
            console.log({result, err})

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
            console.log("Rollback transaction")
            console.log({result, err});
            if (err) {
              reject(err);
            } else {
              resolve(true);
            }
          });
        });
      })*/,
      { numRuns: 10 },
    );
  });
});
