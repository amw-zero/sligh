import { expect } from 'chai';
import 'mocha';
import { Budget as Fullstack,  } from "./generated-client";
import { Budget as Model, RecurringTransaction, CreateRecurringTransaction } from "./generated-model"
import fc from 'fast-check';
import { Database } from "sqlite3";
import { omit } from "lodash";

async function assertSimulation(m: Model, r: Fullstack) {
  // console.log("Before data fetch");
  // console.log(r.recurring_transactions);
  // Compare client side state to model
  expect(r.recurring_transactions).to.deep.eq(m.recurring_transactions);

  // Compare database state to model
  await r.view_recurring_transactions();
  // console.log("After data fetch");
  // console.log(r.recurring_transactions);
  expect(r.recurring_transactions).to.deep.eq(m.recurring_transactions);
}

class CreateCommand implements fc.AsyncCommand<Model, Fullstack> {
  constructor(readonly value: CreateRecurringTransaction) {}
  check = (m: Readonly<Model>) => true;
  async run(m: Model, r: Fullstack): Promise<void> {
    console.log("Create");
    let createdRt = await r.create_recurring_transaction(this.value);
    m.create_recurring_transaction(this.value, createdRt.id);

    return assertSimulation(m, r);
  }
  toString = () => `createRecurringTransaction(${this.value})`;
}

class DeleteCommand implements fc.AsyncCommand<Model, Fullstack> {
  constructor(readonly indexToDelete: number) {}
  check(m: Readonly<Model>): boolean {
    return m.recurring_transactions.length > 0;
  }
  async run(m: Model, r: Fullstack): Promise<void> {
    let toDelete = r.recurring_transactions[this.indexToDelete];
    if (toDelete) {
      console.log("Deleting");
      await r.delete_recurring_transaction(toDelete);
      m.delete_recurring_transaction(toDelete);
    }

    return assertSimulation(m, r);
  }
  toString = () => `deleteRecurringTransaction(${this.indexToDelete})`;
}

describe('FullstackBudget', function() {
  it('simulates the model', async function() {
    const cmds = [
      fc.record({ name: fc.string(), amount: fc.float(), id: fc.integer()}).map(({ name, amount, id }) => {
        let crt = { name, amount };
        return new CreateCommand(crt);
      }),
      fc.record({ indexToDelete: fc.nat({ max: 10 })}).map(({ indexToDelete }) => {
        return new DeleteCommand(indexToDelete);
      }),
    ];

    const db = new Database('../web_server/test.db');

    await fc.assert(
      await fc.asyncProperty(fc.commands(cmds, { maxCommands: 20 }), cmds => {
        const s = () => ({ model: new Model(), real: new Fullstack(() => {}) });
        console.log("\n\n==== Property begin ====")
        let setupPromise = new Promise<boolean | void>((resolve, reject) => {
          db.run("DELETE FROM recurring_transactions", (result, err) => {
            if (err) {
              reject(err);
            } else {
              resolve(fc.asyncModelRun(s, cmds));
            }
          });
        });
        
        return setupPromise;
      }),
      { numRuns: 100 },
    );
  });
});
