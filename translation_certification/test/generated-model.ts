export class RecurringTransaction {
    amount: number = 0;
  name: string = "";
  id: number = 0;
  }

export  class Budget {
    recurring_transactions: RecurringTransaction[] = [];
  create_recurring_transaction(rt: RecurringTransaction) {
    this.recurring_transactions.push(rt) }
  
  delete_recurring_transaction(rt: RecurringTransaction) {
    this.recurring_transactions = this.recurring_transactions.filter((data) => {
    return data.id !== rt.id }) }
  }
