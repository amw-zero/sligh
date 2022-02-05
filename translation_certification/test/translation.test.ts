import { expect } from 'chai';
import 'mocha';
import { Budget as Fullstack,  } from "./generated-client";
import { Budget as Model, RecurringTransaction } from "./generated-model"
import fc from 'fast-check';

class CreateCommand implements fc.AsyncCommand<Model, Fullstack> {
  constructor(readonly value: RecurringTransaction) {}
  check = (m: Readonly<Model>) => true;
  async run(m: Model, r: Fullstack): Promise<void> {
    await r.create_recurring_transactionClient(this.value);
    m.create_recurring_transaction(this.value);

    expect(r.recurring_transactions).to.deep.eq(m.recurring_transactions);
  }
  toString = () => `createRecurringTransaction(${this.value})`;
}
class DeleteCommand implements fc.AsyncCommand<Model, Fullstack> {
  constructor(readonly value: RecurringTransaction) {}
  check(m: Readonly<Model>): boolean {
    return true;
  }
  async run(m: Model, r: Fullstack): Promise<void> {
    r.delete_recurring_transactionClient(this.value)
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
      fc.record({ name: fc.string(), amount: fc.float()}).map(({ name, amount }) => {
        let rt = new RecurringTransaction();
        rt.name = name;
        rt.amount = amount;

        return new DeleteCommand(rt);
      }),
    ];

    await fc.assert(
      fc.asyncProperty(fc.commands(cmds, { maxCommands: 20 }), cmds => {
        const s = () => ({ model: new Model(), real: new Fullstack(() => {}) });
        return fc.asyncModelRun(s, cmds);
      }),
      { numRuns: 25 },
    );
  });
});