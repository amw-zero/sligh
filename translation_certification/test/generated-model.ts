export class RecurringTransaction {
    amount: number = 0;
  name: string = "";

  }
  export class Budget {
    recurring_transactions: RecurringTransaction[] = [];
  create_recurring_transaction(rt: RecurringTransaction) {
    this.recurring_transactions.push(rt) }

  delete_recurring_transaction(rt: RecurringTransaction) {
    this.recurring_transactions = this.recurring_transactions.filter(rtt => rt.name === rtt.name)
    }

//   update_recurring_transaction(rt: RecurringTransaction) {
//     this.recurring_transactions.update!(rt) }

  view_recurring_transactions() {
    this.recurring_transactions }


  }