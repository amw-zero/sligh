export interface RecurringTransaction {
    amount: number;
  name: string;
  id: number;
  }

export interface CreateRecurringTransaction {
    amount: number;
  name: string;
  }

export  class Budget {
    recurring_transactions: RecurringTransaction[] = [];
  create_recurring_transaction(rt: CreateRecurringTransaction, id: number) {
    this.recurring_transactions.push({...rt, id }) }
  
  delete_recurring_transaction(rt: RecurringTransaction) {
    this.recurring_transactions = this.recurring_transactions.filter((data) => {
    return data.id !== rt.id }) }
  }
