import { DBState as DBState, Client as Client, CreateTodo as CreateTodo, Todo as Todo } from "./react_ui/src/state.ts";

import { TodoMVC as TodoMVC } from "./model.ts";

import { assertEquals as assertEquals } from "https://deno.land/std@0.149.0/testing/asserts.ts";

import fc from "https://cdn.skypack.dev/fast-check";

interface AddTodoInput {
 ct: CreateTodo
id: number
}

interface CompleteTodoInput {
 id: number
}

interface UncompleteTodoInput {
 id: number
}

class AddTodoCommand{ct: CreateTodo
id: number
constructor(ct: CreateTodo, id: number) { this.ct = ct;
this.id = id; }
check() { return true }
async run(b: TodoMVC, c: Client) { b.AddTodo(this.ct, this.id)
await c.AddTodo(this.ct, this.id) }}

class CompleteTodoCommand{id: number
constructor(id: number) { this.id = id; }
check() { return true }
async run(b: TodoMVC, c: Client) { b.CompleteTodo(this.id)
await c.CompleteTodo(this.id) }}

class UncompleteTodoCommand{id: number
constructor(id: number) { this.id = id; }
check() { return true }
async run(b: TodoMVC, c: Client) { b.UncompleteTodo(this.id)
await c.UncompleteTodo(this.id) }}

Deno.test("functional correctness", async (t: Deno.TestContext) => {
  let client = new Client()
let AddTodoData = fc.record({ct: fc.record({name: fc.string()}),
id: fc.integer()})
let CompleteTodoData = fc.record({id: fc.integer()})
let UncompleteTodoData = fc.record({id: fc.integer()})
let allCommands = [AddTodoData.map(({ ct: ct, id: id }: AddTodoInput) => {
  return new AddTodoCommand(ct, id)
}), CompleteTodoData.map(({ id: id }: CompleteTodoInput) => {
  return new CompleteTodoCommand(id)
}), UncompleteTodoData.map(({ id: id }: UncompleteTodoInput) => {
  return new UncompleteTodoCommand(id)
})]
await fc.assert(fc.asyncProperty(fc.commands(allCommands, {size: "small"}), async (cmds: any) => {
  await client.setup({recurring_transactions: []})
let model = new TodoMVC([])
client = new Client();
let env = () => {
  return {model: model,
real: client}
}
await fc.asyncModelRun(env, cmds)
assertEquals(client.todos, model.todos)
await client.teardown()
}))
})
