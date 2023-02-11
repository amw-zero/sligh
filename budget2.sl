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
  recurringTransactions: RecurringTransaction
  scheduledTransactions: ScheduledTransaction

  def AddRecurringTransaction(rt: RecurringTransaction):
    recurringTransactions := recurringTransactions.append(rt)
  end
end
