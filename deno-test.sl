def genType(type: Type):
  case type:
    | Schema(s): genSchemaValue(s)
    | String(): tsMethodCall("fc", "string", [])
    | Int(): tsMethodCall("fc", "integer", [])
    | Decimal(): tsMethodCall("fc", "float", [])
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
    | String(): "String"
    | Int(): "Int"
    | Decimal(): "Decimal"
  end
end

def toActionTest(action: Action):
  typescript:
    {{* action.args.map(toTestValue) }}
  end
end

Model.actions.map(toActionTest)
