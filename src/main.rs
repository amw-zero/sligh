use pest::{self, Parser};

#[derive(pest_derive::Parser)]
#[grammar = "grammar.pest"]
struct LangParser;

#[derive(Debug, Clone)]
enum AstNode {
    InvalidNode,
    SchemaDef { name: Box<AstNode>, body: Box<AstNode> },
    SchemaBody {
        definitions: Vec<AstNode>,
    },
    Identifier(String),
    Type(Box<AstNode>),
    SchemaAttribute {
        name: Box<AstNode>,
        r#type: Box<AstNode>,
    },
    SchemaMethod {
        name: Box<AstNode>,
        args: Vec<AstNode>,
        body: Box<AstNode>,
        return_type: Box<AstNode>,
    },
    MethodArgs {
        name: Box<AstNode>,
        arguments: Vec<AstNode>,
    },
    Expr(String),
}

// JS Translation

#[derive(Debug, Clone)]
enum JSAstNode {
    InvalidNode,
    ClassDef { name: Box<JSAstNode>, body: Box<JSAstNode>},
    ClassBody { definitions: Vec<JSAstNode>},
    ClassProperty { identifier: Box<JSAstNode> },
    ClassMethod { name: Box<JSAstNode>, args: Vec<JSAstNode>, body: Box<JSAstNode>},
    Expr(String),
    Identifier(String)
}

fn js_gen_string(node: JSAstNode) -> String {
    match node {
        JSAstNode::ClassDef { name, body } => {
            let class_name = js_gen_iden_name(*name);
            let class_body = js_gen_class_body(*body);
            format!("class {}\n  {}", class_name, class_body)
        },
        _ => "".to_string()
    }
}

fn js_gen_iden_name(node: JSAstNode) -> String {
    match node {
        JSAstNode::Identifier(name) => name,
        _ => panic!("Invalid JS identifier")
    }
}

fn js_gen_class_body(node: JSAstNode) -> String {
    let mut class_body = "".to_string();
    match node {
        JSAstNode::ClassBody{definitions} => {
            for def in definitions {
                match def {
                    JSAstNode::ClassMethod{ name, .. } => {
                        class_body.push_str(format!("{}() {{}}", js_gen_iden_name(*name)).as_str())
                    },
                    _ => ()
                }
            }

            class_body
        },
        _ => panic!("Invalid JS class body")
    }
}

// Note: Doesn't handle types
fn js_translate_attribute(name: AstNode) -> JSAstNode {
    match name {
        AstNode::Identifier(n) => JSAstNode::Identifier(n),
        _ => JSAstNode::InvalidNode
    }
}

fn js_translate_method(name: AstNode, args: Vec<AstNode>, body: AstNode) -> JSAstNode {
    let js_name = js_translate_identifier(name);
    let mut js_args: Vec<JSAstNode> = vec![];
    for arg in args {
        js_args.push(js_translate_attribute(arg));
    }
    let js_body = js_translate_expr(body);

    JSAstNode::ClassMethod { name: Box::new(js_name), args: js_args, body: Box::new(js_body) }
}

fn js_translate(ast: AstNode) -> JSAstNode {
    match ast {
        AstNode::SchemaDef { name, body } => 
            JSAstNode::ClassDef { 
                name: Box::new(js_translate_identifier(*name)), 
                body: Box::new(js_translate_class_body(*body))
            },
        AstNode::SchemaAttribute { name, .. } => js_translate_attribute(*name),
        AstNode::SchemaMethod { name, args, body, .. } => js_translate_method(*name, args, *body),
        _ => JSAstNode::InvalidNode        
    }
}

fn js_translate_class_body(ast: AstNode) -> JSAstNode {
    match ast {
        AstNode::SchemaBody { definitions } => {
            let mut js_definitions: Vec<JSAstNode> = vec![];
            for def in definitions {
                js_definitions.push(js_translate(def));
//                definitions.map(js_translate) 
            }
            JSAstNode::ClassBody { 
                definitions: js_definitions
            }
        }
        _ => JSAstNode::InvalidNode
    }
}

fn js_translate_expr(expr: AstNode) -> JSAstNode {
    match expr {
        AstNode::Expr(e) => JSAstNode::Expr(e),
        _ => JSAstNode::InvalidNode
    }
}

fn js_translate_identifier(ast: AstNode) -> JSAstNode {
    match ast {
        AstNode::Identifier(name) => JSAstNode::Identifier(name),
        _ => JSAstNode::InvalidNode
    }
}

// Parser

fn identifier(pair: pest::iterators::Pair<Rule>) -> AstNode {
    return AstNode::Identifier(pair.as_str().into());
}

fn r#type(pair: pest::iterators::Pair<Rule>) -> AstNode {
    let name = identifier(pair.into_inner().next().unwrap());
    return AstNode::Type(Box::new(name));
}

fn schema_method(pair: pest::iterators::Pair<Rule>) -> AstNode {
    // Only handling methods with arguments right now
    let mut schema_method = pair.into_inner();

    let mut method_args = schema_method.next().unwrap().into_inner();
    let name = parse(method_args.next().unwrap());
    let args = method_args.map(parse).collect();

    let schema_body = schema_method.next().unwrap();
    let body = parse(schema_body);
    
    return AstNode::SchemaMethod { name: Box::new(name), args: args, body: Box::new(body), return_type: Box::new(AstNode::InvalidNode) };
}

fn attribute(pair: pest::iterators::Pair<Rule>) -> AstNode {
    let mut inner = pair.into_inner();
    let name = identifier(inner.next().unwrap());
    let r#type = r#type(inner.next().unwrap());

    return AstNode::SchemaAttribute{ name: Box::new(name), r#type: Box::new(r#type) }
}

fn parse(pair: pest::iterators::Pair<Rule>) -> AstNode {
    // println!("parse - {:?}", pair.as_rule());
    match pair.as_rule() {
        Rule::Statement => parse(pair.into_inner().next().unwrap()),
        Rule::Schema => {
            let mut inner = pair.into_inner();
            let name = identifier(inner.next().unwrap());
            let body = parse(inner.next().unwrap());

            let schema = AstNode::SchemaDef { name: Box::new(name), body: Box::new(body) };
            
            return schema;
        },
        Rule::SchemaBody => {
            let inner = pair.into_inner();
            let definitions = inner.map(parse).collect();

            return AstNode::SchemaBody { definitions: definitions };
        },
        Rule::SchemaAttribute => attribute(pair),
        Rule::SchemaMethod => schema_method(pair),
        Rule::Identifier => AstNode::Identifier(pair.as_str().into()),
        Rule::MethodArgs => {
            return AstNode::MethodArgs { name: Box::new(parse(pair.into_inner().next().unwrap())), arguments: vec![] }
        },
        Rule::MethodBody => parse(pair.into_inner().next().unwrap()),
        Rule::Expr => {
            return AstNode::Expr(pair.as_str().into())
        },
        _ => { 
            println!("Other");
            return AstNode::InvalidNode;
        }
    }
}

fn main() {
    let source = std::fs::read_to_string("./src/test.lang").expect("Gotta exist");
    let result = LangParser::parse(Rule::Program, &source);
//    let mut schemas: HashMap<String, AstNode> = HashMap::new();
    let mut statements: Vec<AstNode> = vec![];
    let mut js: Vec<JSAstNode> = vec![];
    let mut js_code: Vec<String> = vec![];
    match result {
        Ok(pairs) => {
            for pair in pairs {
                let parsed = parse(pair);
                statements.push(parsed.clone());
                let js_ast = js_translate(parsed);
                js.push(js_ast.clone());
                js_code.push(js_gen_string(js_ast));
            }
        },
        Err(e) => println!("Error {:?}", e)
    }

    println!("{:?}", statements);
    println!("{}", js_code[0]);
}
