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
    },
    MethodParams {
        name: Box<AstNode>,
        arguments: Vec<AstNode>,
    },
}

#[derive(Debug, Clone)]
enum JSAstNode {
    InvalidNode,
    ClassDef { name: Box<JSAstNode>, body: Box<JSAstNode>},
    ClassBody { },
    Identifier(String)
}

fn js_gen_string(node: JSAstNode) -> String {
    match node {
        JSAstNode::ClassDef { name, body } => {
            let class_name = js_iden_name(*name);
            let class_body = js_class_body(*body);
            format!("class {}\n  {}", class_name, class_body)
        },
        _ => "".to_string()
    }
}

fn js_iden_name(node: JSAstNode) -> String {
    match node {
        JSAstNode::Identifier(name) => name,
        _ => panic!("Invalid JS identifier")
    }
}

fn js_class_body(node: JSAstNode) -> String {
    match node {
        JSAstNode::ClassBody{} => "".to_string(),
        _ => panic!("Invalid JS class body")
    }
}

fn js_translate(ast: AstNode) -> JSAstNode {
    match ast {
        AstNode::SchemaDef { name, body: _ } => 
            JSAstNode::ClassDef { 
                name: Box::new(js_translate_identifier(name)), 
                body: Box::new(JSAstNode::ClassBody{})
            },
        _ => JSAstNode::InvalidNode        
    }
}

fn js_translate_identifier(ast: Box<AstNode>) -> JSAstNode {
    match *ast {
        AstNode::Identifier(name) => JSAstNode::Identifier(name),
        _ => JSAstNode::InvalidNode
    }
}

fn identifier(pair: pest::iterators::Pair<Rule>) -> AstNode {
    return AstNode::Identifier(pair.as_str().into());
}

fn r#type(pair: pest::iterators::Pair<Rule>) -> AstNode {
    let name = identifier(pair.into_inner().next().unwrap());
    return AstNode::Type(Box::new(name));
}

fn schema_method(pair: pest::iterators::Pair<Rule>) -> AstNode {
    let name = identifier(pair.into_inner().next().unwrap());

    return AstNode::SchemaMethod { name: Box::new(name) };
}

fn attribute(pair: pest::iterators::Pair<Rule>) -> AstNode {
    let mut inner = pair.into_inner();
    let name = identifier(inner.next().unwrap());
    let r#type = r#type(inner.next().unwrap());

    return AstNode::SchemaAttribute{ name: Box::new(name), r#type: Box::new(r#type) }
}

// fn parse_statement(pair: pest::iterators::Pair<Rule>) -> AstNode {
//     match pair.as_rule() {
//         Rule::Schema,
//     }
// }

fn parse(pair: pest::iterators::Pair<Rule>) -> AstNode {
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
        Rule::MethodParams => {
            return AstNode::MethodParams { name: Box::new(parse(pair.into_inner().next().unwrap())), arguments: vec![] }
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

    println!("{}", js_code[0]);
}
