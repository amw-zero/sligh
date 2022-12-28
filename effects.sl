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
    8
  end
end

effect create!(accounts: Account, newAct: Account):
  model:
    accounts.append(newAct)
  end

  impl:
    typescript:
      accounts.push(json)
    end
  end

  server:
    typescript:
      db.exec(5)
    end
  end
end

effect find!(accounts: Account, id: Int):
  model:
    accounts.find(id)
  end

  impl:
    fetch("accounts/id")
  end

  server:
    "server"
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
