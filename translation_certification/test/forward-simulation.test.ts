import { expect } from 'chai';
import 'mocha';
import { Budget as Fullstack,  } from "./generated-client";
import { Budget as Model } from "./generated-model"
import fc from 'fast-check';
import { Database } from "sqlite3";

describe('FullstackBudget', function() {
  this.timeout(60000);
  it('simulates the model', async function() {
    const db = new Database('../web_server/test.db');

    await fc.assert(
      fc.asyncProperty(fc.record({ name: fc.string(), amount: fc.float() }), async (crt) => {
        let model = new Model();
        let fullstack = new Fullstack(() => {});

        let createdRt = await fullstack.create_recurring_transaction(crt);
        model.create_recurring_transaction(crt, createdRt.id);

        expect(fullstack.recurring_transactions).to.deep.eq(model.recurring_transactions);

        await fullstack.view_recurring_transactions();
        expect(fullstack.recurring_transactions).to.deep.eq(model.recurring_transactions);
      }).afterEach(() => {
          return new Promise((resolve, reject) => {
            db.run("DELETE FROM recurring_transactions", (result, err) => {
            if (err) {
              reject(err);
            } else {
              resolve(true);
            }
          });
         });
      }),
      { numRuns: 10000 },
    );
  });
});
