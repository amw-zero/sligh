entity Account:
  balance: Int
  name: String
end

entity Transaction:
  srcAccount: Account
  dstAccount: Account
  amount: Decimal
end

effect create!(accounts: Account, newAct: Account):
  model:
    accounts.append(newAct)
  end

  impl:
    typescript:
      let test = await fetch({{"somethin"}})
      test
    end
  end

  server:
    "server"
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

def toImplMethod(action: Action):
  tsClassMethod(action.name, action.args, action.body)
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
  let classBody = tsClassMethod(
    "constructor", 
    Model.variables.map(toTsTypedAttr), 
    tsStatementList(Model.variables.map(toCtorBodyStmt))
  ).append(
    Model.variables.map(toImplAttr)
      .concat(Model.actions.map(toImplMethod))
  )

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
