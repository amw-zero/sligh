import "mocha";
import { expect } from "chai";
import fc from "fast-check";
import { Budget as Fullstack } from "./generated-client";
import { Budget as Model } from "./generated-model";

function create_recurring_transactionProperty() {
  return fc.asyncProperty(
    fc.record({ amount: fc.float(), name: fc.string() }),
    async (rtc) => {
      let model = new Model();
      let fullstack = new Fullstack(() => {});
      let created = await fullstack.create_recurring_transaction(rtc);
      model.create_recurring_transaction(rtc, created.id);
      expect(fullstack.recurring_transactions).to.deep.eq(
        model.recurring_transactions
      );
    }
  );
}

export const transitionProperties = [
  {
    name: "create_recurring_transaction",
    property: create_recurring_transactionProperty(),
  },
];