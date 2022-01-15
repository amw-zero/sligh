use pest::{self, Parser};
use std::collections::HashMap;

#[derive(pest_derive::Parser)]
#[grammar = "grammar.pest"]
struct LangParser;

#[derive(Debug, Clone)]
enum AstNode {
    InvalidNode,
    SchemaDef {
        name: Box<AstNode>,
        body: Box<AstNode>,
    },
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
    CallExpr {
        receiver: Box<AstNode>,
        call_name: Box<AstNode>,
        args: Vec<AstNode>,
    },
    Expr(String),
}

// JS Translation

#[derive(Debug, Clone)]
struct Prop {
    key: JSAstNode,
    value: JSAstNode,
}

#[derive(Debug, Clone)]
enum JSAstNode {
    InvalidNode,
    ClassDef {
        name: Box<JSAstNode>,
        body: Box<JSAstNode>,
    },
    ClassBody {
        definitions: Vec<JSAstNode>,
    },
    ClassProperty {
        identifier: Box<JSAstNode>,
    },
    ClassMethod {
        name: Box<JSAstNode>,
        args: Vec<JSAstNode>,
        body: Box<JSAstNode>,
    },
    FuncCallExpr {
        call_name: Box<JSAstNode>,
        args: Vec<JSAstNode>,
    },
    CallExpr {
        receiver: Box<JSAstNode>,
        call_name: Box<JSAstNode>,
        args: Vec<JSAstNode>,
    },
    Object {
        props: Vec<Prop>,
    },
    Expr(String),
    Identifier(String),
}

fn js_gen_string(node: JSAstNode) -> String {
    println!("js_gen_string");
    match node {
        JSAstNode::ClassDef { name, body } => {
            println!("JSAstNode::ClassDef");
            let class_name = js_gen_iden_name(*name);
            let class_body = js_gen_class_body(*body);
            format!("class {} {{\n  {}\n}}", class_name, class_body)
        }
        JSAstNode::CallExpr {
            receiver,
            call_name,
            args,
        } => {
            println!("JSAstNode::CallExpr");

            let receiver_name = js_gen_iden_name(*receiver);

            let method_call = js_gen_iden_name(*call_name);
            let mut arg_names: Vec<String> = vec![];
            for arg in args {
                arg_names.push(js_gen_iden_name(arg));
            }
            let comma_separated_args = arg_names.join(", ");

            format!(
                "{}.{}({})",
                receiver_name, method_call, comma_separated_args
            )
        }
        JSAstNode::FuncCallExpr { call_name, args } => {
            println!("JSAstNode::FuncCallExpr");
            let method_call = js_gen_iden_name(*call_name);
            println!("generating arg names");

            let mut arg_names: Vec<String> = vec![];
            for arg in args {
                arg_names.push(js_gen_string(arg));
            }

            let comma_separated_args = arg_names.join(", ");

            format!("{}({})", method_call, comma_separated_args)
        }
        JSAstNode::Identifier(_) => js_gen_iden_name(node),
        JSAstNode::Object { props } => {
            println!("Generating JSAstNode::Object");
            println!("{:?}", props);
            let mut key_values: Vec<String> = vec![];
            for prop in props {
                key_values.push(format!(
                    "{}: {}",
                    js_gen_iden_name(prop.key),
                    js_gen_iden_name(prop.value)
                ))
            }
            format!("{{ {} }}", key_values.join(", "))
        }
        JSAstNode::Expr(e) => e,
        _ => "".to_string(),
    }
}

fn js_gen_iden_name(node: JSAstNode) -> String {
    match node {
        JSAstNode::Identifier(name) => name,
        _ => panic!("Invalid JS identifier"),
    }
}

fn js_gen_class_body(node: JSAstNode) -> String {
    let mut class_body = "".to_string();
    match node {
        JSAstNode::ClassBody { definitions } => {
            for def in definitions {
                match def {
                    JSAstNode::ClassMethod { name, body, .. } => class_body.push_str(
                        format!(
                            "{}() {{ {} }}",
                            js_gen_iden_name(*name),
                            js_gen_string(*body),
                        )
                        .as_str(),
                    ),
                    _ => (),
                }
            }

            class_body
        }
        _ => panic!("Invalid JS class body"),
    }
}

// Note: Doesn't handle types
fn js_translate_attribute(name: AstNode) -> JSAstNode {
    match name {
        AstNode::Identifier(n) => JSAstNode::Identifier(n),
        _ => JSAstNode::InvalidNode,
    }
}

fn js_translate_method(name: AstNode, args: Vec<AstNode>, body: AstNode) -> JSAstNode {
    let js_name = js_translate_identifier(name);
    let mut js_args: Vec<JSAstNode> = vec![];
    for arg in args {
        js_args.push(js_translate_attribute(arg));
    }
    let js_body = js_translate_expr(body);

    JSAstNode::ClassMethod {
        name: Box::new(js_name),
        args: js_args,
        body: Box::new(js_body),
    }
}

fn js_translate(ast: AstNode) -> JSAstNode {
    match ast {
        AstNode::SchemaDef { name, body } => JSAstNode::ClassDef {
            name: Box::new(js_translate_identifier(*name)),
            body: Box::new(js_translate_class_body(*body)),
        },
        AstNode::SchemaAttribute { name, .. } => js_translate_attribute(*name),
        AstNode::SchemaMethod {
            name, args, body, ..
        } => js_translate_method(*name, args, *body),
        _ => JSAstNode::InvalidNode,
    }
}

fn js_translate_class_body(ast: AstNode) -> JSAstNode {
    match ast {
        AstNode::SchemaBody { definitions } => {
            let mut js_definitions: Vec<JSAstNode> = vec![];
            for def in definitions {
                js_definitions.push(js_translate(def));
            }
            JSAstNode::ClassBody {
                definitions: js_definitions,
            }
        }
        _ => JSAstNode::InvalidNode,
    }
}

fn js_translate_expr(expr: AstNode) -> JSAstNode {
    match expr {
        AstNode::CallExpr {
            receiver,
            call_name,
            args,
        } => {
            let mut js_args: Vec<JSAstNode> = vec![];
            for arg in args {
                js_args.push(js_translate_identifier(arg));
            }
            JSAstNode::CallExpr {
                receiver: Box::new(js_translate_identifier(*receiver)),
                call_name: Box::new(js_translate_identifier(*call_name)),
                args: js_args,
            }
        }
        _ => JSAstNode::InvalidNode,
    }
}

fn js_translate_identifier(ast: AstNode) -> JSAstNode {
    match ast {
        AstNode::Identifier(name) => JSAstNode::Identifier(name),
        _ => JSAstNode::InvalidNode,
    }
}

// Infrastructure expansion

fn js_find_class_methods(node: JSAstNode) -> Vec<JSAstNode> {
    let mut class_methods: Vec<JSAstNode> = vec![];
    match node {
        JSAstNode::ClassDef { body, .. } => match *body {
            JSAstNode::ClassBody { definitions } => {
                for def in definitions {
                    match def {
                        JSAstNode::ClassMethod { .. } => class_methods.push(def),
                        _ => (),
                    }
                }
            }
            _ => (),
        },
        _ => (),
    };

    class_methods
}

fn js_class_method_name_expand(method_name: JSAstNode) -> JSAstNode {
    match method_name {
        JSAstNode::Identifier(n) => JSAstNode::Identifier(format!("{}Client", n)),
        _ => JSAstNode::InvalidNode,
    }
}

fn js_state_var_endpoint(state_var: String) -> String {
    format!("\"/{}\"", state_var)
}

fn js_expand_state_transition(_: String, state_var: String, args: Vec<String>) -> JSAstNode {
    // fetch(endpoint_for_state(star_var), fetch_args(args))
    println!("js_expand_state_transition");
    let endpoint = js_state_var_endpoint(state_var);
    let post_prop = Prop {
        key: JSAstNode::Identifier("method".to_string()),
        value: JSAstNode::Identifier("POST".to_string()),
    };
    JSAstNode::FuncCallExpr {
        call_name: Box::new(JSAstNode::Identifier("fetch".to_string())),
        args: vec![
            JSAstNode::Identifier(endpoint),
            JSAstNode::Object {
                props: vec![post_prop],
            },
        ],
    }
}

// Replace class methods (state transitions) with network requests
fn js_class_method_body_expand(body: JSAstNode) -> JSAstNode {
    println!("js_class_method_body_expand");
    match body {
        JSAstNode::CallExpr {
            receiver,
            call_name,
            args,
        } => {
            let receiver_name = js_gen_iden_name(*receiver);
            let call_name = js_gen_iden_name(*call_name);
            let mut arg_names: Vec<String> = vec![];
            println!("Pushing arg names");
            for arg in args {
                arg_names.push(js_gen_iden_name(arg));
            }
            let action = js_expand_state_transition(call_name, receiver_name, arg_names);

            action.clone()
        }
        _ => JSAstNode::InvalidNode,
    }
}

fn js_expand_client(class_method: JSAstNode) -> JSAstNode {
    match class_method {
        JSAstNode::ClassMethod { name, args, body } => {
            let expanded_name = Box::new(js_class_method_name_expand(*name));
            let expanded_body = Box::new(js_class_method_body_expand(*body.clone()));
            JSAstNode::ClassMethod {
                name: expanded_name,
                args: args,
                body: expanded_body,
            }
        }
        _ => JSAstNode::InvalidNode,
    }
}

// might want to write quoted JS macros here:
// consider doing macros in MyLang, i.e. write infra in
// MyLang first before translating to target lang ?. Every
// func call / symbol in MyLang would have to be translated
// by the backend. I.e. client.request() maps to fetch in JS.
fn js_infra_expand(node: JSAstNode) -> JSAstNode {
    let class_methods = js_find_class_methods(node);
    let mut expanded_class_methods: Vec<JSAstNode> = vec![];
    for cm in &class_methods {
        expanded_class_methods.push(js_expand_client(cm.clone()))
    }

    let client = JSAstNode::ClassDef {
        name: Box::new(JSAstNode::Identifier("Client".to_string())),
        body: Box::new(JSAstNode::ClassBody {
            definitions: expanded_class_methods,
        }),
    };

    let server = "class Server { \n".to_string();

    client
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

    return AstNode::SchemaMethod {
        name: Box::new(name),
        args: args,
        body: Box::new(body),
        return_type: Box::new(AstNode::InvalidNode),
    };
}

fn attribute(pair: pest::iterators::Pair<Rule>) -> AstNode {
    let mut inner = pair.into_inner();
    let name = identifier(inner.next().unwrap());
    let r#type = r#type(inner.next().unwrap());

    return AstNode::SchemaAttribute {
        name: Box::new(name),
        r#type: Box::new(r#type),
    };
}

fn parse(pair: pest::iterators::Pair<Rule>) -> AstNode {
    // println!("parse - {:?}", pair.as_rule());
    match pair.as_rule() {
        Rule::Statement => parse(pair.into_inner().next().unwrap()),
        Rule::Schema => {
            let mut inner = pair.into_inner();
            let name = identifier(inner.next().unwrap());
            let body = parse(inner.next().unwrap());

            let schema = AstNode::SchemaDef {
                name: Box::new(name),
                body: Box::new(body),
            };

            schema
        }
        Rule::SchemaBody => {
            let inner = pair.into_inner();
            let definitions = inner.map(parse).collect();

            AstNode::SchemaBody {
                definitions: definitions,
            }
        }
        Rule::SchemaAttribute => attribute(pair),
        Rule::SchemaMethod => schema_method(pair),
        Rule::Identifier => AstNode::Identifier(pair.as_str().into()),
        Rule::MethodDefArgs => AstNode::MethodArgs {
            name: Box::new(parse(pair.into_inner().next().unwrap())),
            arguments: vec![],
        },
        Rule::MethodBody => parse(pair.into_inner().next().unwrap()),
        Rule::Expr => {
            let mut expr = pair.into_inner();
            AstNode::CallExpr {
                receiver: Box::new(AstNode::Identifier(expr.next().unwrap().as_str().into())),
                call_name: Box::new(AstNode::Identifier(expr.next().unwrap().as_str().into())),
                args: expr
                    .map(|e| AstNode::Identifier(e.as_str().into()))
                    .collect(),
            }
        }
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
    let mut js_code: Vec<String> = vec![];
    let mut js_infra_expanded: Vec<JSAstNode> = vec![];
    let mut js_infra_code: Vec<String> = vec![];
    let mut js_asts: Vec<JSAstNode> = vec![];

    match result {
        Ok(pairs) => {
            for pair in pairs {
                let parsed = parse(pair);

                statements.push(parsed.clone());
                let js_ast = js_translate(parsed.clone());
                js_asts.push(js_ast.clone());
                js_code.push(js_gen_string(js_ast.clone()));
                let infra_expanded = js_infra_expand(js_ast);
                js_infra_expanded.push(infra_expanded.clone());
                println!("generating expanded infra string");
                js_infra_code.push(js_gen_string(infra_expanded));
            }
        }
        Err(e) => println!("Error {:?}", e),
    }

    for c in js_asts {
        println!("JS: {:?}", c);
        println!("Class methods: {:?}\n", js_find_class_methods(c));
    }

    println!("JS Translation:\n");
    for c in js_code {
        println!("{}", c);
    }

    println!("\n\nInfrastructure Expansion:\n");
    for ex in js_infra_code {
        println!("{}", ex)
    }
}
