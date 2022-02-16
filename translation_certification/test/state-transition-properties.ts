import 'mocha';
import { expect } from "chai";
import fc from "fast-check";
import { Budget as Fullstack,  } from "./generated-client";
import { Budget as Model } from "./generated-model"

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
  
export const transitionProperties = [
    { name: "createRecurringTransaction", property: createRecurringTransactionsProperty() }
];