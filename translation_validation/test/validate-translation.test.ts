import { expect } from 'chai';
import 'mocha';
import { Budget, RecurringTransaction } from "./generated-client";

describe('Array', function() {
  describe('#indexOf()', function() {
    it('should return -1 when the value is not present', function() {
      let budget = new Budget(() => {});
      let rt = new RecurringTransaction(() => {});
      rt.name = "test";
      rt.amount = 50.0;

      budget.create_recurring_transactionClient(rt);
      expect(budget.recurring_transactions[0].name).to.eq("test");
    });
  });
});