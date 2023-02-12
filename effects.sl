entity Account:
  balance: Int
  name: String
end

entity Transaction:
  srcAccount: Account
  dstAccount: Account
  amount: Decimal
end

process Accounts:
  accounts: Account

  def OpenAccount(newAct: Account):
    accounts.create!(newAct)
  end

  def FindAccount(id: Int):
    accounts.find!(id)
  end
end

process Ledger:
  transactions: Transaction

  def Transfer(srcAct: Account, dstAct: Account, amount: Decimal):
    srcAct.guardBalance!(amount)

    srcAct.update!(srcAct.balance)
    dstAct.update!(dstAct.balance)
    transactions.create!(Transaction("transfer", srcAct, dstAct, amount))
  end
end

rewrite guardBalance!:
  model(arg: Int):
    if srcAct.balance.lessThan(amount):
      6
    else:
      5
    end
  end
end

rewrite create!:
  model(accounts: Account, newAct: Account):
    accounts.append(newAct)
  end

  impl(accounts: Identifier, newAct: Identifier):
    typescript: 5 end
  end

  server(accounts: Account, newAct: Account):
    typescript:
      db.exec(5)
    end
  end
end

rewrite find!:
  model(accounts: Account, id: Int):
    accounts.find(id)
  end

  impl(accounts: Account, id: Int):
    typescript: 
      fetch("accounts/id")
    end
  end

  server(accounts: Account, id: Int):
    typescript:
      "server"
    end
  end
end

def toImplMethod(action: Action):
  let actionBody = typescript:
    let resp = fetch({{ action.name }}, { 
      method: "POST", 
      body: JSON.stringify({{ tsIden(action.args.index(0).name) }}) 
    })
    let json = await resp.json()

    {{ action.body }}
  end

  tsClassMethod(action.name, action.args, actionBody)
end

def toImplAttr(attr: TypedAttribute):
  tsClassProp(attr.name, attr.type)
end

def toTsTypedAttr(attr: TypedAttribute):
  tsTypedAttr(attr.name, attr.type)
end

def toCtorBodyStmt(attr: TypedAttribute):
  let attrIden = tsIden(attr.name)
  let target = tsAccess(tsIden("this"), attrIden)

  tsAssignment(target, attrIden)
end

def clientClass():
  let constructor = tsClassMethod(
    "constructor", 
    Model.variables.map(toTsTypedAttr), 
    tsStatementList(Model.variables.map(toCtorBodyStmt))
  )
  let classDefs = Model.variables.map(toImplAttr)
      .concat(Model.actions.map(toImplMethod))
  let classBody = constructor.append(classDefs)

  tsClass("Client", classBody)
end

def toTsInterface(schema: Schema):
  let attrs = schema.attributes.map(toTsTypedAttr)

  tsInterface(schema.name, attrs)
end

implementation:
  typescript:
    {{* Model.schemas.map(toTsInterface) }}
    {{ clientClass() }}
  end
end

def toServerEndpoint(action: Action):
  let endpointBody = tsClosure([tsIden("req"), tsIden("resp")], action.body)

  tsMethodCall("app", "post", [action.name, endpointBody])
end     

file server:
  typescript:
    {{* Model.actions.map(toServerEndpoint) }}
  end
end
