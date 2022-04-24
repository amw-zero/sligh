interface RecurringTransaction { id: number;amount: number;name: string }

interface CreateRecurringTransaction { amount: number;name: string }

interface ScheduledTransaction { name: string }

interface ScenarioRecurringTransaction { id: number;amount: number;name: string }

interface Scenario { id: number;name: string;scenarioRecurringTransactions: ScenarioRecurringTransaction[] }

interface CreateScenarioRequest { name: string }

interface CreateScenarioRecurringTransaction { amount: number;name: string }

interface CreateScenario { name: string;scenarioRecurringTransactions: CreateScenarioRecurringTransaction[] }

function expand(rt: RecurringTransaction) {
  return { name: rt.name };

}

function toScenarioTransaction(recurringTransaction: RecurringTransaction) {
  return { amount: recurringTransaction.amount, name: recurringTransaction.name };

}

export function defineEndpoints(app, db) {
  app.post("/recurringTransactions", (req, res) => {
 let data = req.body;
db.serialize(() => {
 db.run("INSERT INTO recurring_transactions (amount, name) VALUES (?, ?)", [data.amount, data.name]);
db.get("SELECT last_insert_rowid()", (err, row) => {
 res.send({ amount: data.amount, name: data.name, id: row["last_insert_rowid()"] }) });
 });
 });
app.delete("/recurringTransactions/:id", (req, res) => {
 db.run("DELETE FROM recurring_transactions WHERE id = ?", [req.params.id]);
res.send({  });
 });
app.put("/recurringTransactions/:id", (req, res) => {
 res.send({  }) });
app.get("/recurringTransactions", (req, res) => {
 db.all("SELECT * FROM recurring_transactions", (_, rows) => {
 res.send(rows);
 }) });
app.get("/scheduledTransactions", (req, res) => {
 db.all("SELECT * FROM recurring_transactions", (_, rts) => {
 let scheduledTransactions = rts.map(expand);
res.send(scheduledTransactions);
 }) });
app.post("/scenarios", (req, res) => {
 let data = req.body;
db.serialize(() => {
 db.run("INSERT INTO scenarios (name) VALUES (?)", [data.name]);
db.get("SELECT last_insert_rowid()", (err, row) => {
 res.send({ name: data.name, scenarioRecurringTransactions: [], id: row["last_insert_rowid()"] }) });
 });
 });

}