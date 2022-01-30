function defineEndpoints(app, db) {
    app.post("/recurring_transactions", (req, res) => {
      let data = req.body;
     db.run("INSERT INTO recurring_transactions (amount, name) VALUES (?, ?)", [data.amount, data.name]);
     res.send({  });
    });
  
  
    app.get("/recurring_transactions", (req, res) => { 
      alasql("INSERT SUM SQL")
  
      // SELECT * FROM recurring_transactions
    });
  
    app.put("/recurring_transactions", (req, res) => { 
      alasql("INSERT SUM SQL") 
      // UPDATE recurring_transactions SET 
    })
    app.delete("/recurring_transactions", (req, res) => { 
  
      // DELETE FROM recurring_transactions
      alasql("INSERT SUM SQL") 
    })
}
  
module.exports = defineEndpoints;
