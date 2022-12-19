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

      5
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
    typescript: 7 end
  end
end

process Accounts:
  accounts: Account

  def OpenAccount(newAct: Account):
    accounts.create!(newAct)
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
  let this = tsIden("this")
  let attrIden = tsIden(attr.name)
  let target = tsAccess(this, attrIden)

  tsAssignment(target, attrIden)
end

def clientClass():
  let methods = Model.actions.map(toImplMethod)
  let attrs = Model.variables.map(toImplAttr)
  
  let ctorArgs = Model.variables.map(toTsTypedAttr)
  let ctorBody = Model.variables.map(toCtorBodyStmt)
  let ctorStatements = tsStatementList(ctorBody)
  let ctor = tsClassMethod("constructor", ctorArgs, ctorStatements)

  let defs = attrs.concat(methods)
  let nextDefs = append(ctor, defs)

  tsClass("Client", nextDefs)
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
  let req = tsIden("req")
  let resp = tsIden("resp")
  let closureArgs = [req, resp]
  let endpointBody = tsClosure(closureArgs, action.body)

  tsMethodCall("app", "post", [action.name, endpointBody])
end     

file server:
  typescript:
    {{* Model.actions.map(toServerEndpoint) }}
  end
end
