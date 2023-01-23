def genString():
  tsMethodCall("fc", "string", [])
end

def genInt():
  tsMethodCall("fc", "integer", [])
end

def genFloat():
  tsMethodCall("fc", "float", [])
end

def genType(type: Type):
  case type:
    | Schema(s): genSchemaValue(s)
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
    | Int(): tsLet(attr.name, genInt())
    | Decimal(): tsLet(attr.name, genFloat())
  end
end

def toCallValue(arg: TypedAttribute):
  tsIden(arg.name)
end

def toActionTest(action: Action):
  let clientName = "client"
  let dataSetup = action.args.map(toTestValue)
  let property = [tsAwait(
    tsMethodCall("fc", "assert", [
      tsMethodCall("fc", "asyncProperty", [tsAsync(
        tsClosure([tsTypedAttr("state", tsType("State"))], [
          tsLet("client", tsNew("Client", [])),
          tsLet("model", tsNew("Budget", [])),
          tsLet("cresp", tsAwait(tsMethodCall(clientName, "setup", [tsIden("state.db")]))),
          tsAwait(tsMethodCall("cresp", "arrayBuffer", [])),
          tsAwait(tsMethodCall(clientName, action.name, action.args.map(toCallValue))),
          tsAwait(tsMethodCall("client", "teardown", []))
        ])
      )])
    ])
  )]

  let testBody = dataSetup.concat(property)
  let testWrapper = tsClosure([tsTypedAttr("t", tsType("Deno.Test"))], testBody).tsAsync()
  
  tsMethodCall("Deno", "test", [action.name, testWrapper])
end

typescript:
  {{* Model.actions.map(toActionTest) }}
end
