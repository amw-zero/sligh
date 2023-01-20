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
    srcAct.update!(srcAct.balance)
    dstAct.update!(dstAct.balance)
    transactions.create!(Transaction("transfer", srcAct, dstAct, amount))
  end
end
