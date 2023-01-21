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

def toActionTest(action: Action):
  let dataSetup = action.args.map(toTestValue)
  let testBody = tsClosure([tsTypedAttr("t", tsType("Deno.Test"))], dataSetup)
  
  tsMethodCall("Deno", "test", [action.name, testBody])
end

typescript:
  {{* Model.actions.map(toActionTest) }}
end
