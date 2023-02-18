def genString():
  tsMethodCall("fc", "string", [])
end

def genInt():
  tsMethodCall("fc", "integer", [])
end

def genFloat():
  tsMethodCall("fc", "float", [])
end

def genVariantCaseAttr(attr: TypedAttribute):
  tsObjectProp(attr.name, genType(attr.type))
end

def genVariantCase(vc: VariantCase):
  tsMethodCall("fc", "record", [tsObject(vc.attrs.map(genVariantCaseAttr))])
end

def genVariant(name: String, cases: VariantCaseList):
  tsMethodCall("fc", "oneOf", cases.map(genVariantCase))
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
    | String(): genString()
    | Int(): genInt()
    | Decimal(): genFloat()
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

def toTestValue(attr: TypedAttribute):
  case attr.type:
    | Schema(s): tsLet(attr.name, genSchemaValue(s))
    | String(): tsLet(attr.name, genString())
    | Generic(name, types): tsLet(attr.name, genGeneric(name, types))
    | Int(): tsLet(attr.name, genInt())
    | Decimal(): tsLet(attr.name, genFloat())
  end
end

def toCallValue(arg: TypedAttribute):
  tsIden("state.".appendStr(arg.name))
end

def actionStateTypeName(actionName: String):
  actionName.appendStr("State")
end

def toTsTypedAttr(attr: TypedAttr):
  tsTypedAttr(attr.name, attr.type)
end

def actionState(action: Action):
  action.args.concat(action.stateVars)
end

def toActionStateType(action: Action):
  tsInterface(actionStateTypeName(action.name),
    actionState(action).map(toTsTypedAttr))
end

def toStateProp(attr: TypedAttribute):
  tsObjectProp(attr.name, tsIden(attr.name))
end

def toActionTest(action: Action):
  let clientName = "client"
  let dataSetup = actionState(action).map(toTestValue)
  let stateSetup = tsLet("state", tsObject(actionState(action).map(toStateProp)))

  let property = [tsAwait(
    tsMethodCall("fc", "assert", [
      tsMethodCall("fc", "asyncProperty", [tsIden("state"),
        tsClosure([tsTypedAttr("state", tsType(actionStateTypeName(action.name)))], [
          tsLet("client", tsNew("Client", [])),
          tsLet("model", tsNew("Budget", [])),
          tsLet("cresp", tsAwait(tsMethodCall(clientName, "setup", [tsIden("state.db")]))),
          tsAwait(tsMethodCall("cresp", "arrayBuffer", [])),
          tsAwait(tsMethodCall(clientName, action.name, action.args.map(toCallValue))),
          tsMethodCall("model", action.name, action.args.map(toCallValue)),

          tsAwait(tsMethodCall("client", "teardown", []))
        ], true)
      ])
    ])
  )]

  let testBody = [dataSetup, [stateSetup], property].flatten()
  let testWrapper = tsClosure([tsTypedAttr("t", tsType("Deno.Test"))], testBody, false)
  
  tsMethodCall("Deno", "test", [action.name, testWrapper])
end

def actionTests():
  tsClosure([], Model.actions.map(toActionTest), false)
end

def toSchemaImplImport(schema: Schema):
  tsSymbolImport(schema.name, schema.name.appendStr("Impl"))
end

typescript:
  {{ tsAliasImport(
    [tsSymbolImport("one", "two")], "file") }}

  {{ tsDefaultImport("some", "file") }}
  {{ tsLet("x", typescript: {{ [1,2,3].length() }} end) }}

  {{ tsAliasImport(Model.schemas.map(toSchemaImplImport), "./react_ui/src/state.ts") }}
  {{* Model.actions.map(toActionStateType) }}
  {{ tsExport(tsLet("runTests", actionTests())) }}
end
