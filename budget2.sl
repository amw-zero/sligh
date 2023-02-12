data RecurrenceRule:
  | Weekly(day: Int, other: Int)
  | Monthly(day: Int)
end

entity RecurringTransaction:
  name: String
  amount: Decimal
  rule: RecurrenceRule
end

entity ScheduledTransaction:
  name: String
  date: Int
end

process Budget:
  recurringTransactions: Set(RecurringTransaction)
  scheduledTransactions: Set(ScheduledTransaction)

  def AddRecurringTransaction(rt: RecurringTransaction):
    recurringTransactions := recurringTransactions.append(rt)
  end

  def DeleteRecurringTransaction(id: Int):
    recurringTransactions := recurringTransactions.delete(id)
  end
end
