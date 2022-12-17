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
    typescript: 5 end
  end
end

effect find!(accounts: Account, id: Int):
  model:
    accounts.find(id)
  end

  impl:
    fetch("accounts/id")
  end
end

process Accounts:
  accounts: Account

  def OpenAccount(newAct: Account):
    accounts.create!(newAct)
  end

  def UpdateBalance(act: Account, balance: Decimal):
    6
  end

  def ViewAccount(id: Int):
    accounts.find!(id)
  end
end

process Ledger:
  transactions: Transaction

  def Transfer(srcAct: Account, dstAct: Account, amount: Decimal):
    7
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

def impl():
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
    {{ impl() }}
  end
end

file server:
  typescript: let x = {{ "test" }} end
end
