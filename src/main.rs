use pest::{self, Parser};
use std::cmp::Ordering;

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
        definitions: Vec<AstNode>, // Vec<SchemaAttribute | SchemaMethod>
    },
    Identifier(String),
    TypedIdentifier {
        identifier: Box<AstNode>, // Identifier
        r#type: Box<AstNode>,     // Identifier
    },
    Type(Box<AstNode>),
    SchemaAttribute {
        typed_identifier: Box<AstNode>, // TypedIdentifier
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
    ArrowClosure {
        args: Vec<JSAstNode>,
        body: Box<JSAstNode>,
    },
    StringLiteral(String),
    Expr(String),
    Identifier(String),
}

// Top level function for converting a JS statement into a string
fn js_gen_string(node: JSAstNode) -> String {
    println!("Generating sttring for {:?}", node);
    match node {
        JSAstNode::ClassDef { name, body } => {
            let class_name = js_gen_iden_name(*name);
            let class_body = js_gen_string(*body);
            format!("class {} {{\n  {}\n}}", class_name, class_body)
        }
        JSAstNode::ClassBody { definitions } => {
            let mut class_body = "".to_string();
            for def in definitions {
                class_body.push_str(&js_gen_string(def));
            }

            class_body
        }
        JSAstNode::ClassMethod { name, body, .. } => format!(
            "{}() {{ {} }}\n\n",
            js_gen_iden_name(*name),
            js_gen_string(*body),
        ),
        JSAstNode::ClassProperty { identifier } => {
            let name = js_gen_iden_name(*identifier);
            format!("{};\n", name)
        }
        JSAstNode::CallExpr {
            receiver,
            call_name,
            args,
        } => {
            let receiver_name = js_gen_iden_name(*receiver);

            let method_call = js_gen_iden_name(*call_name);
            let mut arg_strs: Vec<String> = vec![];
            for arg in args {
                arg_strs.push(js_gen_string(arg));
            }
            let comma_separated_args = arg_strs.join(", ");

            format!(
                "{}.{}({})",
                receiver_name, method_call, comma_separated_args
            )
        }
        JSAstNode::FuncCallExpr { call_name, args } => {
            let method_call = js_gen_iden_name(*call_name);

            let mut arg_strs: Vec<String> = vec![];
            for arg in args {
                arg_strs.push(js_gen_string(arg));
            }

            let comma_separated_args = arg_strs.join(", ");

            format!("{}({})", method_call, comma_separated_args)
        }
        JSAstNode::Identifier(_) => js_gen_iden_name(node),
        JSAstNode::Object { props } => {
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
        JSAstNode::ArrowClosure { args, body } => {
            let mut arg_strs: Vec<String> = vec![];
            for arg in args {
                arg_strs.push(js_gen_string(arg));
            }
            let comma_separated_args = arg_strs.join(", ");
            let body_str = js_gen_string(*body);
            format!("({}) => {{ {} }}", comma_separated_args, body_str)
        }
        JSAstNode::StringLiteral(s) => format!("\"{}\"", s),
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
fn js_translate_method(name: AstNode, args: Vec<AstNode>, body: AstNode) -> JSAstNode {
    let js_name = js_translate(name);
    let mut js_args: Vec<JSAstNode> = vec![];
    for arg in args {
        js_args.push(js_translate(arg));
    }
    let js_body = js_translate(body);

    JSAstNode::ClassMethod {
        name: Box::new(js_name),
        args: js_args,
        body: Box::new(js_body),
    }
}

fn js_translate(ast: AstNode) -> JSAstNode {
    match ast {
        AstNode::SchemaDef { name, body } => JSAstNode::ClassDef {
            name: Box::new(js_translate(*name)),
            body: Box::new(js_translate(*body)),
        },
        AstNode::SchemaAttribute { typed_identifier } => JSAstNode::ClassProperty {
            identifier: Box::new(js_translate(*typed_identifier)),
        },
        AstNode::TypedIdentifier { identifier, .. } => JSAstNode::Identifier(match *identifier {
            AstNode::Identifier(n) => n,
            _ => panic!("Expected Identifier node"),
        }),
        AstNode::Identifier(n) => JSAstNode::Identifier(n),
        AstNode::SchemaMethod {
            name, args, body, ..
        } => js_translate_method(*name, args, *body),
        AstNode::SchemaBody { definitions } => {
            let mut js_definitions: Vec<JSAstNode> = vec![];
            for def in definitions {
                js_definitions.push(js_translate(def));
            }
            JSAstNode::ClassBody {
                definitions: js_definitions,
            }
        }
        AstNode::CallExpr {
            receiver,
            call_name,
            args,
        } => {
            let mut js_args: Vec<JSAstNode> = vec![];
            for arg in args {
                js_args.push(js_translate(arg));
            }
            JSAstNode::CallExpr {
                receiver: Box::new(js_translate(*receiver)),
                call_name: Box::new(js_translate(*call_name)),
                args: js_args,
            }
        }
        _ => JSAstNode::InvalidNode,
    }
}

// Infrastructure expansion

struct PartitionedClassDefinitions {
    class_methods: Vec<JSAstNode>,
    other_definitions: Vec<JSAstNode>,
}

fn js_ast_node_cmp(l: &JSAstNode, r: &JSAstNode) -> Ordering {
    match (l, r) {
        (JSAstNode::ClassProperty { .. }, JSAstNode::ClassMethod { .. }) => Ordering::Less,
        (JSAstNode::ClassMethod { .. }, JSAstNode::ClassProperty { .. }) => Ordering::Greater,
        (JSAstNode::ClassProperty { .. }, JSAstNode::ClassProperty { .. }) => Ordering::Equal,
        (JSAstNode::ClassMethod { .. }, JSAstNode::ClassMethod { .. }) => Ordering::Equal,
        _ => panic!("Should only be sorting class properties and methods"),
    }
}

fn js_partition_class_definitions(node: JSAstNode) -> PartitionedClassDefinitions {
    let mut class_methods: Vec<JSAstNode> = vec![];
    let mut other_definitions: Vec<JSAstNode> = vec![];
    match node {
        JSAstNode::ClassDef { body, .. } => match *body {
            JSAstNode::ClassBody { definitions } => {
                for def in definitions {
                    match def {
                        JSAstNode::ClassMethod { .. } => class_methods.push(def),
                        JSAstNode::ClassProperty { .. } => other_definitions.push(def),
                        _ => panic!("Found unknown class body definition"),
                    }
                }
            }
            _ => (),
        },
        _ => (),
    };

    PartitionedClassDefinitions {
        class_methods: class_methods,
        other_definitions: other_definitions,
    }
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

// TODO: Add post body
fn js_expand_state_transition_client(_: String, state_var: String, args: Vec<String>) -> JSAstNode {
    // fetch(endpoint_for_state(star_var), fetch_args(args))
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
    match body {
        JSAstNode::CallExpr {
            receiver,
            call_name,
            args,
        } => {
            let receiver_name = js_gen_iden_name(*receiver);
            let call_name = js_gen_iden_name(*call_name);
            let mut arg_names: Vec<String> = vec![];
            for arg in args {
                arg_names.push(js_gen_iden_name(arg));
            }
            let action = js_expand_state_transition_client(call_name, receiver_name, arg_names);

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

fn js_make_client(class_defs: &PartitionedClassDefinitions) -> JSAstNode {
    let mut expanded_definitions: Vec<JSAstNode> = vec![];
    for cm in &class_defs.class_methods {
        expanded_definitions.push(js_expand_client(cm.clone()))
    }
    for cm in &class_defs.other_definitions {
        expanded_definitions.push(cm.clone());
    }

    expanded_definitions.sort_by(js_ast_node_cmp);

    // Quoted macro version:
    // quote: class Client {
    //   `expanded_definitions`
    // }
    JSAstNode::ClassDef {
        name: Box::new(JSAstNode::Identifier("Client".to_string())),
        body: Box::new(JSAstNode::ClassBody {
            definitions: expanded_definitions,
        }),
    }
}

// We generate a set of endpoints corresponding to all each state transition
fn js_make_server(class_defs: &PartitionedClassDefinitions) -> Vec<JSAstNode> {
    let mut endpoints: Vec<JSAstNode> = vec![];
    for class_method in &class_defs.class_methods {
        endpoints.push(js_expand_endpoint(class_method.clone()));
    }

    endpoints
}

// Expand state transitions into SQL queries inside of server
fn js_expand_class_method_to_endpoint(body: JSAstNode) -> JSAstNode {
    match body {
        JSAstNode::CallExpr {
            receiver,
            call_name,
            args,
        } => {
            println!("Generatin state var name");
            let state_var = js_gen_iden_name(*receiver);
            println!("Generated state var name");
            let endpoint_path = js_state_var_endpoint(state_var);

            // This needs to be a closure containing the SQL query
            let endpoint_body = JSAstNode::ArrowClosure {
                args: vec![
                    JSAstNode::Identifier("req".to_string()),
                    JSAstNode::Identifier("res".to_string()),
                ],
                body: Box::new(JSAstNode::FuncCallExpr {
                    call_name: Box::new(JSAstNode::Identifier("alasql".to_string())),
                    args: vec![JSAstNode::StringLiteral("INSERT SUM SQL".to_string())],
                }),
            };

            JSAstNode::CallExpr {
                receiver: Box::new(JSAstNode::Identifier("app".to_string())),
                call_name: Box::new(JSAstNode::Identifier("get".to_string())),
                args: vec![JSAstNode::Identifier(endpoint_path), endpoint_body],
            }
        }
        _ => panic!("Unexpected JSAstNode type, should only be expanding CallExprs"),
    }
}

// must create one endpoint per state var:
// app.get('/recurring_transactions', (req, res) => {
//   alasql("INSERT INTO recurring_transactions VALUES (50.0, 'mortgage')");
//   res.send('Hello World!');
// })
// JSAstNodes: arrow closure
fn js_expand_endpoint(class_method: JSAstNode) -> JSAstNode {
    match class_method {
        JSAstNode::ClassMethod { body, .. } => js_expand_class_method_to_endpoint(*body),
        _ => JSAstNode::InvalidNode,
    }
}

// might want to write quoted JS macros here:
// consider doing macros in MyLang, i.e. write infra in
// MyLang first before translating to target lang ?. Every
// func call / symbol in MyLang would have to be translated
// by the backend. I.e. client.request() maps to fetch in JS.
fn js_infra_expand(node: JSAstNode) -> (JSAstNode, Vec<JSAstNode>) {
    let partitioned_class_defs = js_partition_class_definitions(node);
    let client = js_make_client(&partitioned_class_defs);
    let server = js_make_server(&partitioned_class_defs);

    (client, server)
}

// Parser

fn identifier(pair: pest::iterators::Pair<Rule>) -> AstNode {
    return AstNode::Identifier(pair.as_str().into());
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

fn parse(pair: pest::iterators::Pair<Rule>) -> AstNode {
    // Debug
    println!("Parsing");
    println!("{}", pair.to_json());
    match pair.as_rule() {
        Rule::Statement => parse(pair.into_inner().next().unwrap()),
        Rule::TypedIdentifier => {
            let mut inner = pair.into_inner();
            AstNode::TypedIdentifier {
                identifier: Box::new(parse(inner.next().unwrap())),
                r#type: Box::new(parse(inner.next().unwrap())),
            }
        }
        Rule::SchemaType => AstNode::Identifier(pair.as_str().into()),
        Rule::Type => AstNode::Type(Box::new(parse(pair.into_inner().next().unwrap()))),
        Rule::SchemaDef => {
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
        Rule::SchemaAttribute => AstNode::SchemaAttribute {
            typed_identifier: Box::new(parse(pair.into_inner().next().unwrap())),
        },
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
                let (client, server) = js_infra_expand(js_ast);
                js_infra_expanded.push(client.clone());
                js_infra_expanded.append(&mut server.clone());

                println!("generating expanded infra string");
                js_infra_code.push(js_gen_string(client));
                for endpoint in server {
                    js_infra_code.push(js_gen_string(endpoint));
                }
            }
        }
        Err(e) => println!("Error {:?}", e),
    }

    // Debug
    println!("Parsed statements");
    for s in statements {
        println!("{:?}", s);
    }

    println!("JS Translation:\n");
    for c in js_code {
        println!("{}", c);
    }

    println!("\n\nInfrastructure Expansion:\n");
    for ex in &js_infra_code {
        println!("{}", ex)
    }

    let web_requires = "const express = require('express');\n\
        const app = express();\n\
        const port = 3000;\n";

    let instantiate_server = "const server = new Server();\n";
    let web_listen = "app.listen(port, () => {\n\
        console.log(`Example app listening at http://localhost:${port}`)\n\
      })\n";
    let web_server = format!(
        "{}{}\n{}{}",
        web_requires, js_infra_code[1], instantiate_server, web_listen
    );

    println!("\n\nWeb server:\n");
    println!("{}", web_server);
}
