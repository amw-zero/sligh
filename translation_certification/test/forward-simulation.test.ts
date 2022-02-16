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
});

after((done) => {
  server.close(done);
});

[1,2,3].forEach((i) => {
  describe(`Test ${i}`, () => {
    it ("idk", () => {
      expect(true).to.eq(true);
    })
  });
});

function createRecurringTransactionsProperty() {
  // derive generators from schema defs
  return fc.asyncProperty(fc.record({ name: fc.string(), amount: fc.float() }), async (crt) => {
    // TODO: Also generate starting states for the classes to test starting from
    // arbitrary starting points.
    let model = new Model();
    let fullstack = new Fullstack(() => {});

    // derive action name from state transition
    let createdRt = await fullstack.create_recurring_transaction(crt);
    model.create_recurring_transaction(crt, createdRt.id);
        
    expect(fullstack.recurring_transactions).to.deep.eq(model.recurring_transactions);

    // make sure to call read!s
    await fullstack.view_recurring_transactions();

    expect(fullstack.recurring_transactions).to.deep.eq(model.recurring_transactions);
  })
}

type StateTransitionTest = {
  name: string;
  property: any;
};

const tests: StateTransitionTest[] = [
  { name: "createRecurringTransactions", property: createRecurringTransactionsProperty() }
];

tests.forEach(({ name, property}) => {
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
