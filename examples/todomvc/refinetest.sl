def genVariantCaseAttr(attr: TypedAttribute):
  tsObjectProp(attr.name, genType(attr.type))
end

def genVariantCase(vc: VariantCase):
  tsMethodCall("fc", "record", [
    tsObject(vc.attrs.map(genVariantCaseAttr).append(tsObjectProp("type", tsMethodCall("fc", "constant", [tsString(vc.name)]))))
  ])
end

def genVariant(name: String, cases: VariantCaseList):
  tsMethodCall("fc", "oneof", cases.map(genVariantCase))
end

def genGeneric(name: String, types: Array(Type)):
  case name:
    | "Set": tsMethodCall("fc", "uniqueArray", [genType(types.index(0))])
  end
end

def genType(type: Type):
  case type:
    | Schema(s): genSchemaValue(s)
    | Variant(name, cases): genVariant(name, cases)
    | Generic(name, types): genGeneric(name, types)
    | String(): tsMethodCall("fc", "string", [])
    | Int(): tsMethodCall("fc", "integer", [])
    | Decimal(): tsMethodCall("fc", "float", [])
  end
end

def genTypeValueObjProp(attr: TypedAttribute):
  tsObjectProp(attr.name, genType(attr.type))
end

def genSchemaValue(s: Schema):
  tsMethodCall(
    "fc",
    "record",
    [tsObject(s.attributes.map(genTypeValueObjProp))]
  )
end

def toArgDataSetup(arg: TypedAttr):
  tsObjectProp(arg.name, genType(arg.type))
end

def actionData(action: Action):
  action.name.appendStr("Data")
end

def commandDataSetup(action: Action):
  tsLet(
    actionData(action),
    tsMethodCall("fc", "record", [
      tsObject(action.args.map(toArgDataSetup))
    ])
  )
end

def toName(arg: TypedAttr):
  tsIden(arg.name)
end

def toMapObjectPatProp(arg: TypedAttr):
  tsObjectPatProp(arg.name, arg.name)
end

def toMapArg(action: Action):
  tsObjectPat(action.args.map(toMapObjectPatProp), tsType(actionInputTypeName(action)))
end

def commandInstantiation(action: Action):
  tsMethodCall(actionData(action), "map", [
    tsClosure([
      toMapArg(action)
    ], [
      tsReturn(tsNew(commandName(action), action.args.map(toName)))
    ], false)]
  )
end

def toCommandInstantiation(action: Action):
  if action.args.length().greaterThan(0):
    commandInstantiation(action)
  else:
    tsMethodCall("fc", "constant", [tsNew(commandName(action), [])])
  end
end

def commandData():
  Model.actions.map(commandDataSetup)
end

def toStateAssertion(var: TypedAttr):
  tsFuncCall("assertEquals", [tsIden("client.".appendStr(var.name)), tsIden("model.".appendStr(var.name))])
end

def funcorrectTest():
  let funcorrectProperty = [
    tsAwait(tsMethodCall("client", "setup", [
      tsObject([tsObjectProp("recurring_transactions", [])])
    ])),
    tsLet("model", tsNew("TodoMVC", [[]])),
    tsAssignment(tsIden("client"), tsNew("Client", [])),
    tsLet("env", tsClosure([], [
      tsReturn(
        tsObject([
          tsObjectProp("model", tsIden("model")),
          tsObjectProp("real", tsIden("client"))
        ])
      )
    ], false)),
    tsAwait(tsMethodCall("fc", "asyncModelRun", [tsIden("env"), tsIden("cmds")]))
  ].concat(Model.variables.map(toStateAssertion)).concat([
    tsAwait(tsMethodCall("client", "teardown", []))
  ])

  let testBody = [
    tsLet("client", tsNew("Client", []))
  ].concat(commandData()).concat([
    tsLet("allCommands", Model.actions.map(toCommandInstantiation)),
    tsAwait(
      tsMethodCall("fc", "assert", [
        tsMethodCall("fc", "asyncProperty", [
          tsMethodCall("fc", "commands", [
            tsIden("allCommands"),
            tsObject([tsObjectProp("size", tsString("small"))])
          ]),
          tsClosure([tsTypedAttr("cmds", tsType("any"))], funcorrectProperty, true)
        ])
      ])
    )
  ])

  tsMethodCall("Deno", "test", [
    "functional correctness",
    tsClosure([tsTypedAttr("t", tsType("Deno.TestContext"))], testBody, true)
  ])
end

def toTypedAttr(arg: TypedAttr):
  tsTypedAttr(arg.name, arg.type)
end

def toArgAssignment(arg: TypedAttr):
  tsAssignment(tsIden("this.".appendStr(arg.name)), tsIden(arg.name))
end

def toActionArg(arg: TypedAttr):
  tsIden("this.".appendStr(arg.name))
end

def actionMethod(action: Action):
  tsClassMethod("run", [tsTypedAttr("b", tsType("TodoMVC")), tsTypedAttr("c", tsType("Client"))], [
    tsMethodCall("b", action.name, action.args.map(toActionArg)),
    tsAwait(tsMethodCall("c", action.name, action.args.map(toActionArg)))
  ], true)
end

def commandName(action: Action):
  action.name.appendStr("Command")
end

def toClassProp(arg: TypedAttr):
  tsClassProp(arg.name, arg.type)
end

def toActionCommandClass(action: Action):
  tsClass(commandName(action), action.args.map(toClassProp).concat([
    tsClassMethod("constructor", action.args.map(toTypedAttr), action.args.map(toArgAssignment), false),
    tsClassMethod("check", [], [tsReturn(tsIden("true"))], false),
    actionMethod(action)
  ]))
end

def toSchemaImplImport(schema: Schema):
  tsSymbolImport(schema.name, schema.name)
end

def imports():
  [
    tsAliasImport(
      Model.schemas.map(toSchemaImplImport)
        .append(tsSymbolImport("Client", "Client"))
        .append(tsSymbolImport("DBState", "DBState")),
      "./react_ui/src/state.ts"),
    tsAliasImport([
      tsSymbolImport("TodoMVC", "TodoMVC")
    ], "./model.ts"),
    tsAliasImport(
      [tsSymbolImport("assertEquals", "assertEquals")],
      "https://deno.land/std@0.149.0/testing/asserts.ts"),
    tsDefaultImport("fc", "https://cdn.skypack.dev/fast-check")
  ]
end

def actionInputTypeName(action: Action):
  action.name.appendStr("Input")
end

def toActionInputType(action: Action):
  tsInterface(actionInputTypeName(action), action.args.map(toTypedAttr))
end

typescript:
  {{* imports() }}
  {{* Model.actions.map(toActionInputType) }}
  {{* Model.actions.map(toActionCommandClass) }}
  {{ funcorrectTest() }}
end
