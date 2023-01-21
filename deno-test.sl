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
  let genRecord = tsMethodCall(
    "fc",
    "record",
    [tsObject(s.attributes.map(genTypeValueObjProp))]
  )

  tsLet(s.name, genRecord)
end

def toTestValue(attr: TypedAttribute):
  case attr.type:
    | Schema(s): genSchemaValue(s)
    | String(): genString()
    | Int(): genInt()
    | Decimal(): genFloat()
  end
end

def toActionTest(action: Action):
  let dataSetup = action.args.map(toTestValue)
  let testBody = tsClosure([tsTypedAttr("t", tsType("Deno.Test"))], dataSetup)
  
  tsMethodCall("Deno", "test", [testBody])
end

typescript:
  {{* Model.actions.map(toActionTest) }}
end
