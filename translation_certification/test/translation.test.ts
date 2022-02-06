import { expect } from 'chai';
import 'mocha';
import { Budget as Fullstack,  } from "./generated-client";
import { Budget as Model, RecurringTransaction } from "./generated-model"
import fc from 'fast-check';
import { Database } from "sqlite3";
import { omit } from "lodash";

function withoutId(rt: RecurringTransaction): Omit<RecurringTransaction, "id"> {
  return omit(rt, "id");
}

class CreateCommand implements fc.AsyncCommand<Model, Fullstack> {
  constructor(readonly value: RecurringTransaction) {}
  check = (m: Readonly<Model>) => true;
  async run(m: Model, r: Fullstack): Promise<void> {
    await r.create_recurring_transaction(this.value);
    await r.view_recurring_transactions();
    m.create_recurring_transaction(this.value);

    console.log("Fullstack:")
    console.log(r.recurring_transactions);
    console.log("Model:")
    console.log(m.recurring_transactions);
    expect(r.recurring_transactions.map(withoutId)).to.deep.eq(m.recurring_transactions.map(withoutId));
  }
  toString = () => `createRecurringTransaction(${this.value})`;
}
class DeleteCommand implements fc.AsyncCommand<Model, Fullstack> {
  constructor(readonly value: RecurringTransaction) {}
  check(m: Readonly<Model>): boolean {
    return true;
  }
  async run(m: Model, r: Fullstack): Promise<void> {
    await r.delete_recurring_transaction(this.value)
    await r.view_recurring_transactions();
    m.delete_recurring_transaction(this.value);


    expect(r.recurring_transactions).to.deep.eq(m.recurring_transactions);
  }
  toString = () => `deleteRecurringTransaction(${this.value})`;
}

describe('FullstackBudget', function() {
  it('simulates the model', async function() {
    const cmds = [
      fc.record({ name: fc.string(), amount: fc.float()}).map(({ name, amount }) => {
        let rt = new RecurringTransaction();
        rt.name = name;
        rt.amount = amount;

        return new CreateCommand(rt);
      }),
      // fc.record({ name: fc.string(), amount: fc.float()}).map(({ name, amount }) => {
      //   let rt = new RecurringTransaction();
      //   rt.name = name;
      //   rt.amount = amount;

      //   return new DeleteCommand(rt);
      // }),
    ];

    const db = new Database('../web_server/test.db');

    await fc.assert(
      fc.asyncProperty(fc.commands(cmds, { maxCommands: 20 }), cmds => {
        console.log("\n\n==== Property iteration ====");
        const s = () => ({ model: new Model(), real: new Fullstack(() => {}) });
        let setupPromise = new Promise<boolean | void>((resolve, reject) => {
          db.run("DELETE FROM recurring_transactions", (result, err) => {
            console.log("Data setup")
            if (err) {
              console.log("Data error");
              reject(err);
            } else {
              console.log("Running model");
              resolve(fc.asyncModelRun(s, cmds));
            }
          });
        });
        
        return setupPromise;
      }),
      { numRuns: 25 },
    );
  });
});