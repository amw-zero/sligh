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
    recurringTransactions.create!(rt)
  end

  def DeleteRecurringTransaction(id: Int):
    recurringTransactions.delete!(id)
  end
end
