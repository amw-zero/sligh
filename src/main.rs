use pest::{self, Parser};
use std::cmp::Ordering;

const DEBUG: bool = false;
const API_HOST: &str = "localhost:3000";

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
        receiver: Box<JSAstNode>,  // Identifier
        call_name: Box<JSAstNode>, // Identifier
        args: Vec<JSAstNode>,
    },
    Object {
        props: Vec<Prop>,
    },
    ArrowClosure {
        args: Vec<JSAstNode>,
        body: Box<JSAstNode>,
    },
    StatementList {
        statements: Vec<JSAstNode>
    },
    StringLiteral(String),
    Expr(String),
    Identifier(String),
    TypedIdentifier {
        identifier: Box<JSAstNode>, // Identifier,
        r#type: Box<JSAstNode>     // Identifier
     }
}

// Top level function for converting a JS statement into a string
fn js_gen_string(node: JSAstNode) -> String {
    if DEBUG {
        println!("Generating string for {:?}", node);
    }
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
        JSAstNode::ClassMethod { name, body, args } => {
            let mut arg_strs: Vec<String> = vec![];
            for arg in args {
                arg_strs.push(js_gen_string(arg));
            }
            let comma_separated_args = arg_strs.join(", ");
            format!(
                "{}({}) {{\n  {} }}\n\n",
                js_gen_iden_name(*name),
                comma_separated_args,
                js_gen_string(*body),
            )
        }
        JSAstNode::StatementList { statements } => {
            let mut stmt_strs: Vec<String> = vec![];
            for s in statements {
                stmt_strs.push(js_gen_string(s));
            }

            format!("{};\n", stmt_strs.join(";\n  "))
        }
        JSAstNode::ClassProperty { identifier } => {
            let property = js_gen_string(*identifier);
            format!("{};\n", property)
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
        JSAstNode::TypedIdentifier { identifier, r#type } => {
            let iden_str = js_gen_string(*identifier);
            let type_str = js_gen_string(*r#type);

            let type_str_chars: Vec<char> = type_str.chars().collect();
            let last_char = *type_str_chars.last().unwrap();
            let num_chars = type_str_chars.len();
            let gen_type_str = if type_str_chars[0] == '[' && last_char == ']' {
                let type_name: String = type_str_chars.into_iter().skip(1).take(num_chars - 2).collect();
                format!("{}[]", type_name)
            } else {
                type_str
            };

            format!("{}: {}", iden_str, gen_type_str)
        }
        JSAstNode::Identifier(_) => js_gen_iden_name(node),
        JSAstNode::Object { props } => {
            let mut key_values: Vec<String> = vec![];
            for prop in props {
                key_values.push(format!(
                    "{}: {}",
                    js_gen_iden_name(prop.key),
                    js_gen_string(prop.value)
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
        AstNode::TypedIdentifier { identifier, r#type } => JSAstNode::TypedIdentifier {
            identifier: Box::new(js_translate(*identifier)),
            r#type: Box::new(js_translate(*r#type))
        },
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
    state_transitions: Vec<JSAstNode>,
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
    let mut state_transitions: Vec<JSAstNode> = vec![];
    let mut other_definitions: Vec<JSAstNode> = vec![];
    match node {
        JSAstNode::ClassDef { body, .. } => match *body {
            JSAstNode::ClassBody { definitions } => {
                for def in definitions {
                    match def.clone() {
                        JSAstNode::ClassMethod { body, .. } => {
                            match *body {
                                JSAstNode::CallExpr { call_name, .. } => {
                                    // The convention is that method names ending in ! are
                                    // state transitions. Separate these for subsequent expansion
                                    let method_name = js_gen_iden_name(*call_name);
                                    let name_chars: Vec<char> = method_name.chars().collect();
                                    if *name_chars.last().unwrap() == '!' {
                                        state_transitions.push(def);
                                    } else {
                                        other_definitions.push(def);
                                    }
                                },
                                _ => other_definitions.push(def)
                            }
                        }
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
        state_transitions: state_transitions,
        other_definitions: other_definitions,
    }
}

fn js_class_method_name_expand(method_name: JSAstNode) -> JSAstNode {
    match method_name {
        JSAstNode::Identifier(n) => JSAstNode::Identifier(format!("{}Client", n)),
        _ => JSAstNode::InvalidNode,
    }
}

fn js_state_var_endpoint_client(state_var: &String) -> String {
    format!("{}/{}", API_HOST, state_var)
}

fn js_state_var_endpoint_server(state_var: String) -> String {
    format!("/{}", state_var)
}

enum StateTransitionFunc {
    Create,
    Update,
    Delete,
}

impl StateTransitionFunc {
    fn as_http_method(&self) -> &'static str {
        match self {
            StateTransitionFunc::Create => "POST",
            StateTransitionFunc::Update => "PUT",
            StateTransitionFunc::Delete => "DELETE",
        }
    }
}

fn state_transition_func_from_str(s: &str) -> StateTransitionFunc {
    match s {
        "create!" => StateTransitionFunc::Create,
        "update!" => StateTransitionFunc::Update,
        "delete!" => StateTransitionFunc::Delete,
        _ => panic!(format!("Unexpected StateTransitionFunc string: {}", s))
    }
}

fn js_expand_fetch_args_from_state_transition(st: &StateTransitionFunc, state_var_iden: &JSAstNode) -> JSAstNode {
    let body_prop = Prop {
        key: JSAstNode::Identifier("body".to_string()),

        // TODO: Only JSON.stringifying one method call argument here
        value: JSAstNode::CallExpr {
            receiver: Box::new(JSAstNode::Identifier("JSON".to_string())),
            call_name: Box::new(JSAstNode::Identifier("stringify".to_string())),
            args: vec![state_var_iden.clone()],
        },
    };
    let post_prop = Prop {
        key: JSAstNode::Identifier("method".to_string()),
        value: JSAstNode::StringLiteral(st.as_http_method().to_string()),
    };

    JSAstNode::Object {
        props: vec![post_prop, body_prop],
    }
}

fn js_expand_update_client_state_from_state_transition(st: &StateTransitionFunc, state_var_iden: &JSAstNode, state_var: &str) -> JSAstNode {
    match st {
         _ => JSAstNode::CallExpr {
            receiver: Box::new(JSAstNode::Identifier(format!("this.{}", state_var))),
            call_name: Box::new(JSAstNode::Identifier("push".to_string())),
            args: vec![state_var_iden.clone()]
        }
    }
    // StateTransitionFunc::Create        
    //        StateTransitionFunc::Update =>
    //        StateTransitionFunc::Delete =>
}

// This turns a semantic state transition into a network request to update the state in
// the database as well as optimistically update the client state
fn js_expand_state_transition_client(call_name: String, state_var: String, args: Vec<String>) -> JSAstNode {
    let endpoint = js_state_var_endpoint_client(&state_var);
    let state_var_iden = JSAstNode::Identifier(args[0].clone());
    let state_trans_func = state_transition_func_from_str(&call_name);
    let st_fetch_args = js_expand_fetch_args_from_state_transition(&state_trans_func, &state_var_iden);

    let fetch_args = vec![
        JSAstNode::StringLiteral(endpoint),
        st_fetch_args,
    ];

    let fetch = JSAstNode::FuncCallExpr {
        call_name: Box::new(JSAstNode::Identifier("fetch".to_string())),
        args: fetch_args,
    };
    let update_client_state = js_expand_update_client_state_from_state_transition(&state_trans_func, &state_var_iden, &state_var);

    JSAstNode::StatementList {
        statements: vec![fetch, update_client_state]
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

fn js_make_client(class_name: String, class_defs: &PartitionedClassDefinitions) -> JSAstNode {
    let mut expanded_definitions: Vec<JSAstNode> = vec![];
    for st in &class_defs.state_transitions {
        expanded_definitions.push(js_expand_client(st.clone()))
    }
    for cm in &class_defs.other_definitions {
        expanded_definitions.push(cm.clone());
    }    

    expanded_definitions.sort_by(js_ast_node_cmp);

    // This may not belong here - but here is where the constructor for the top-level
    // client state object is.
    let config_func_type = format!("(a: {}) => void", class_name);
    let constructor = JSAstNode::ClassMethod {
        name: Box::new(JSAstNode::Identifier("constructor".to_string())),
        args: vec![
            JSAstNode::TypedIdentifier {
                identifier: Box::new(JSAstNode::Identifier("config".to_string())),
                r#type: Box::new(JSAstNode::Identifier(config_func_type))
            }
        ],
        body: Box::new(JSAstNode::FuncCallExpr {
            args: vec![
                JSAstNode::Identifier("this".to_string())
            ],
            call_name: Box::new(JSAstNode::Identifier("config".to_string()))
        })
    };

    expanded_definitions.insert(0, constructor);

    // Quoted macro version:
    // quote: class Client {
    //   `expanded_definitions`
    // }
    JSAstNode::ClassDef {
        name: Box::new(JSAstNode::Identifier(class_name)),
        body: Box::new(JSAstNode::ClassBody {
            definitions: expanded_definitions,
        }),
    }
}

// We generate a set of endpoints corresponding to all each state transition
fn js_make_server(class_defs: &PartitionedClassDefinitions) -> Vec<JSAstNode> {
    let mut endpoints: Vec<JSAstNode> = vec![];
    for st in &class_defs.state_transitions {
        endpoints.push(js_expand_endpoint(st.clone()));
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
            let state_var = js_gen_iden_name(*receiver);
            let endpoint_path = js_state_var_endpoint_server(state_var);

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

            // TODO: Only generating POST endpoints - have to infer HTTP method from
            // statement semantics
            JSAstNode::CallExpr {
                receiver: Box::new(JSAstNode::Identifier("app".to_string())),
                call_name: Box::new(JSAstNode::Identifier("post".to_string())),
                args: vec![JSAstNode::StringLiteral(endpoint_path), endpoint_body],
            }
        }
        _ => panic!("Unexpected JSAstNode type, should only be expanding CallExprs"),
    }
}

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
    match node {
        JSAstNode::ClassDef { ref name, .. } => {
            let partitioned_class_defs = js_partition_class_definitions(node.clone());
            // TODO: I don't know why &** is necessary here
            let class_name = match &**name {
                JSAstNode::Identifier(n) => n,
                _ => panic!("Expected identifier"),
            };
            let client = js_make_client(class_name.clone(), &partitioned_class_defs);
            let server = js_make_server(&partitioned_class_defs);

            (client, server)
        }
        _ => (JSAstNode::InvalidNode, vec![]),
    }
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
    if DEBUG {
        println!("Parsing");
        println!("{}", pair.to_json());
    }
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
    let source_file = std::env::args().nth(1).expect("No source file specified");
    let source = std::fs::read_to_string(source_file).expect("Gotta exist");
    let result = LangParser::parse(Rule::Program, &source);
    let mut statements: Vec<AstNode> = vec![];
    let mut js_code: Vec<String> = vec![];
    let mut js_infra_expanded: Vec<JSAstNode> = vec![];
    let mut js_infra_code: Vec<String> = vec![];
    let mut js_asts: Vec<JSAstNode> = vec![];
    let mut endpoint_strs: Vec<String> = vec![];
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

                js_infra_code.push(js_gen_string(client));
                for endpoint in server {
                    let endpoint_str = js_gen_string(endpoint);
                    js_infra_code.push(endpoint_str.clone());
                    endpoint_strs.push(endpoint_str);
                }
            }
        }
        Err(e) => println!("Error {:?}", e),
    }

    // Debug
    /*
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
    */

    if DEBUG {
        println!("JS Translation:\n");
        println!("{:?}\n", js_asts[2]);
    }

    println!("Client code:\n");
    for c in js_infra_code {
        println!("{}", c);
    }

    let web_requires = "const express = require('express');\n\
        const app = express();\n\
        const port = 3000;\n\
        app.use(cors({options: \"*\"}));\n\
        app.use(express.json());\n";

    let web_listen = "app.listen(port, () => {\n\
        console.log(`Example app listening at http://localhost:${port}`)\n\
      })\n";
    let all_endpoints = endpoint_strs.join("\n");
    let web_server = format!("{}{}\n{}", web_requires, all_endpoints, web_listen);

    println!("\n\nWeb server:\n");
    println!("{}", web_server);
}
