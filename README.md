# Sligh

`Sligh` is a language that compiles a specification of essential logic into a full-stack web application, and certifies that both are semantically equivalent. 

Here is the specification of some simple CRUD functionality for a budgeting application, written in `Sligh`:

```
schema RecurringTransaction:
  id: Int
  amount: Numeric
  name: String
end

schema CreateRecurringTransaction:
  amount: Numeric
  name: String
end

domain Budget:
  recurring_transactions: [RecurringTransaction]
  
  def create_recurring_transaction(rtc: CreateRecurringTransaction)
    recurring_transactions.create!(rtc)
  end
  
  def delete_recurring_transaction(rt: RecurringTransaction)
    recurring_transactions.delete!(rt)
  end

  def update_recurring_transaction(rt: RecurringTransaction)
    recurring_transactions.update!(rt)
  end

  def read_recurring_transactions()
    recurring_transactions.read!()
  end
end
```
Note that what's described here is very minimal: just the schema of datatypes and the essence of CRUD behavior via calls to `create!()`, `read!()`, `update!()`, and `delete!()`. These are special calls that `Sligh` deems "state transitions" because they signal where system state should actually change.

From this specification, the `Sligh` compiler generates several files for embedding this logic into a web application. It currently assumes that the client code will be embedded into a React application, the server code will be embedded into an Express.js application, and the underlying database is SQLite. I'm hoping to make this more flexible in the future by having different technological "backends," but these were chosen to build out the proof of concept of the language and compiler.

Here is an excerpt of the generated code, focusing on the `create_recurring_transaction` action:

(**client.ts**)
```
interface RecurringTransaction {
  id: number;
  amount: number;
  name: string;
}

interface CreateRecurringTransaction {
  amount: number;
  name: string;
}

class Budget {
  constructor(config: (a: Budget) => void) {
    config(this);
  }
  
  recurring_transactions: RecurringTransaction[] = [];
  
  async create_recurring_transaction(rtc: CreateRecurringTransaction) {
    let resp = await fetch("http://localhost:3000/recurring_transactions", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(rtc),
    });
    this.recurring_transactions.push(await resp.json());
  }
  
  ...
}
```


(**server.js**)
```
 app.post("/recurring_transactions", (req, res) => {
    let data = req.body;
    db.serialize(() => {
      db.run(
        "INSERT INTO recurring_transactions (amount, name) VALUES (?, ?)",
        [data.amount, data.name]
      );
      db.get("SELECT last_insert_rowid()", (err, row) => {
        res.send({
          amount: data.amount,
          name: data.name,
          id: row["last_insert_rowid()"],
        });
      });
    });
  });

```

`Sligh` also cares about correctness, and beyond generating purely behavioral code it generates the components of a test to certify that the full-stack implementation, with all of its additional technical details, has the equivalent semantics of the specification. It does this by generating a property-based test which checks that the implementation is a forward simulation of the model:


(**model.ts**)
```
interface RecurringTransaction {
  id: number;
  amount: number;
  name: string;
}

interface CreateRecurringTransaction {
  amount: number;
  name: string;
}

class Budget {
  recurring_transactions: RecurringTransaction[] = [];
  
  create_recurring_transaction(rtc: CreateRecurringTransaction) {
    this.recurring_transactions.push(rtc);
  }
  
  ...
}
```

(**certification-properites.ts**)
```
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
```

Forward simulation essentially means that, for all state transitions, the result of the transition is the same in both the model and the implementation.

# Examples

[Recur](https://github.com/amw-zero/recur_app) is an in-progress personal finance budgeting application being built using `Sligh`. This has an example of how the generated client-side code is integrated into a React application, and how the generated endpoints are integrated into an Express.js application.


# Technical Info
Technically, `Sligh` is a language with a certifying source-to-source compiler. It is source-to-source because it compiles to Typescript and Javascript, and it is certifying because it generates a test that can be run independently to informally verify that the target code is semantically equivalent to the source model.

# Future

Typescript + Javascript were chosen as the target languages for convenience and simplicity. My hope is to eventually compile to WebAssembly.

I also want to support the formal verification of semantic equivalence, though this is a much more ambitious goal. WebAssembly is an enabler of that though, because of its [official formal semantics](https://www.repository.cam.ac.uk/handle/1810/329032).


# Prior Art / Inspiration

I don't think there are any real new ideas in `Sligh`, but the packaging together of certain ideas and some of the technological choices are probably a little unique.

Full credit to all of the following language projects that I've learned so much from:

* [Links](https://homepages.inf.ed.ac.uk/slindley/papers/links-fmco06.pdf)
* [Cogent](https://cogent.readthedocs.io/en/latest/)
* [CompCert](https://compcert.org/compcert-C.html)
* [Ur-Web](http://www.impredicative.com/ur/)
* [Svelte](https://svelte.dev/)

Also, my love of simple specification came from:

* [TLA+](https://lamport.azurewebsites.net/tla/tla.html)
* [Alloy](https://alloytools.org/)

The most relevant papers to read are:

* [A formally verified compiler back-end](https://www.cs.cmu.edu/~15811/papers/compcert-journal.pdf)
* [The Cogent case for property-based testing](https://trustworthy.systems/publications/csiro_full_text/Chen_OKKH_17.pdf)

# Status

`Sligh` is experimental in nature, and currently very immature. 