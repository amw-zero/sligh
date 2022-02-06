import "isomorphic-fetch";
import { RecurringTransaction } from "./generated-model";

  export class Budget {
    constructor(config: (a: Budget) => void) {
    config(this) }
  
  recurring_transactions: RecurringTransaction[] = [];
  
  async create_recurring_transaction(rt: RecurringTransaction) {
    let resp = await fetch("http://localhost:3000/recurring_transactions", { method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify(rt) });
    console.log({createResponse: resp});
  this.recurring_transactions.push(rt);
   }
  
  async delete_recurring_transaction(rt: RecurringTransaction) {
    fetch("http://localhost:3000/recurring_transactions/" + rt.id, { method: "DELETE", headers: { "Content-Type": "application/json" } });
  this.recurring_transactions = this.recurring_transactions.filter((data) => {
   return data.id !== rt.id });
   }
  
  async update_recurring_transaction(rt: RecurringTransaction) {
    ;
   }
  
  async view_recurring_transactions() {
    console.log("View recurring transactions");
    let data = await fetch("http://localhost:3000/recurring_transactions", { method: "GET", headers: { "Content-Type": "application/json" } });
    let json = await data.json();
  this.recurring_transactions = json;
   }
}

