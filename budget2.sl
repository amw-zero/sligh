data RecurrenceRule:
  | Weekly(day: Int, other: Int)
  | Monthly(day: Int)
end

entity RecurringTransaction:
  id: Int
  name: String
  amount: Decimal
  rule: RecurrenceRule
end

entity CreateRecurringTransaction:
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

  def AddRecurringTransaction(crt: RecurringTransaction, id: Int):
    recurringTransactions := recurringTransactions.append(RecurringTransaction.new(
      id, crt.name, crt.amount, crt.rule
    ))
  end

  def DeleteRecurringTransaction(id: Int):
    recurringTransactions := recurringTransactions.delete(id)
  end
end
