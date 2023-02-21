entity Todo:
  id: Int
  name: String
  isCompleted: Bool
end

entity CreateTodo:
  name: String
end

process TodoMVC:
  todos: Set(Todo)

  def AddTodo(ct: CreateTodo, id: Int):
    todos := todos.append(Todo.new(id, ct.name, false))
  end

  def CompleteTodo(id: Int):
    def markComplete(t: Todo):
      if t.id.equals(id):
        Todo.new(t.id, t.name, true)
      else:
        t
      end
    end

    todos := todos.map(markComplete)
  end

  def UncompleteTodo(id: Int):
    def markUncomplete(t: Todo):
      if t.id.equals(id):
        Todo.new(t.id, t.name, true)
      else:
        t
      end
    end

    todos := todos.map(markUncomplete)
  end
end