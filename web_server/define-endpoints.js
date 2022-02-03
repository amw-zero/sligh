function defineEndpoints(app, db) {
    app.post("/recurring_transactions", (req, res) => {
      let data = req.body;
     db.run("INSERT INTO recurring_transactions (amount, name) VALUES (?, ?)", [data.amount, data.name]);
     res.send({  });
    });
}
  
module.exports = defineEndpoints;
