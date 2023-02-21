export interface Todo {
	id: number
name: string
isCompleted: boolean
}

export interface CreateTodo {
	name: string
}

export class TodoMVC {
 constructor(todos: Array<Todo>) {
  this.todos = todos;
}
  todos: Array<Todo>
AddTodo(ct: CreateTodo, id: number) {
	this.todos = 
    (() => {
    let a = [...this.todos];
    a.push({ id: id, name: ct.name, isCompleted: false });

    return a;
    })();
    
}
CompleteTodo(id: number) {
	function markComplete(t: Todo) {
	let ret = 
      (() => {
        if (
    t.id === id
    ) {
          return { id: t.id, name: t.name, isCompleted: true }
        } else {
          return t
        }
      })()
      ; return ret;
}

this.todos = 
    this.todos.map((a) => markComplete(a))
    
}
UncompleteTodo(id: number) {
	function markUncomplete(t: Todo) {
	let ret = 
      (() => {
        if (
    t.id === id
    ) {
          return { id: t.id, name: t.name, isCompleted: true }
        } else {
          return t
        }
      })()
      ; return ret;
}

this.todos = 
    this.todos.map((a) => markUncomplete(a))
    
}
}
