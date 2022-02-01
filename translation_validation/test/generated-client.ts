import "isomorphic-fetch";
import { RecurringTransaction } from "./generated-model";

  export class Budget {
    constructor(config: (a: Budget) => void) {
    config(this) }
  
  recurring_transactions: RecurringTransaction[] = [];
  
  create_recurring_transactionClient(rt: RecurringTransaction) {
    fetch("http://localhost:3000/recurring_transactions", { method: "POST", body: JSON.stringify(rt), headers: { "Content-Type": "application/json" } });
  this.recurring_transactions.push(rt);
   }
  
  delete_recurring_transactionClient(rt: RecurringTransaction) {
    fetch("http://localhost:3000/recurring_transactions", { method: "DELETE", body: JSON.stringify(rt), headers: { "Content-Type": "application/json" } });
  this.recurring_transactions = this.recurring_transactions.filter(rtt => rtt.name === rt.name);
   }
  
  update_recurring_transactionClient(rt: RecurringTransaction) {
    fetch("http://localhost:3000/recurring_transactions", { method: "PUT", body: JSON.stringify(rt), headers: { "Content-Type": "application/json" } });
  this.recurring_transactions.push(rt);
   }
  
  view_recurring_transactions() {
    this.recurring_transactions }
  
  
  }

