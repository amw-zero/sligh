data RecurrenceRule:
  | Weekly(day: Int, other: Int)
  | Monthly(day: Int)
end

entity RecurringTransaction:
  name: String
  amount: Decimal
  rule: RecurrenceRule
end

process Budget:
  recurringTransactions: RecurringTransaction

  def AddRecurringTransaction(rt: RecurringTransaction):
    recurringTransactions := recurringTransactions.append(rt)
  end

  def DeleteRecurringTransaction(id: Int): 
    recurringTransactsions := recurringTransactons.delete(id)
  end
end
