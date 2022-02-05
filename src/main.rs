use pest::{self, Parser};
use std::cmp::Ordering;
use std::collections::HashMap;

const DEBUG: bool = false;
const API_HOST: &str = "http://localhost:3000";

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
    // Expr(String),
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
        typed_identifier: Box<JSAstNode>,
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
        statements: Vec<JSAstNode>,
    },
    ReturnStatement(Box<JSAstNode>),
    AsyncModifier {
        node: Box<JSAstNode>, // ClassMethod
    },
    AwaitOperator {
        node: Box<JSAstNode>, // FuncCallExpr or CallExpr of an async-modified ClassMethod def
    },
    LetExpr {
        name: Box<JSAstNode>,  // Identifier,
        value: Box<JSAstNode>, // any node
    },
    AssignmentStatement {
        left: Box<JSAstNode>,  // Identifier
        right: Box<JSAstNode>, // any node
    },
    PlusExpr {
        left: Box<JSAstNode>,
        right: Box<JSAstNode>,
    },
    EqualityExpr {
        left: Box<JSAstNode>,
        right: Box<JSAstNode>,
    },
    NotEqualExpr {
        left: Box<JSAstNode>,
        right: Box<JSAstNode>,
    },
    ArrayLiteral(Vec<JSAstNode>),
    StringLiteral(String),
    // Expr(String),
    Identifier(String),
    TypedIdentifier {
        identifier: Box<JSAstNode>, // Identifier,
        r#type: Box<JSAstNode>,     // Identifier
    },
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
        JSAstNode::AsyncModifier { node } => {
            format!("async {}", js_gen_string(*node))
        }
        JSAstNode::AwaitOperator { node } => {
            format!("await {}", js_gen_string(*node))
        }
        JSAstNode::StatementList { statements } => {
            let mut stmt_strs: Vec<String> = vec![];
            for s in statements {
                stmt_strs.push(js_gen_string(s));
            }

            format!("{};\n", stmt_strs.join(";\n"))
        }
        JSAstNode::ReturnStatement(e) => format!("return {}", js_gen_string(*e)),
        JSAstNode::LetExpr { name, value } => {
            let name_str = js_gen_string(*name);
            let value_str = js_gen_string(*value);

            format!("let {} = {}", name_str, value_str)
        }
        JSAstNode::AssignmentStatement { left, right } => {
            format!("{} = {}", js_gen_string(*left), js_gen_string(*right))
        }
        JSAstNode::ClassProperty { typed_identifier } => {
            let property = js_gen_string(*typed_identifier);
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
                let type_name: String = type_str_chars
                    .into_iter()
                    .skip(1)
                    .take(num_chars - 2)
                    .collect();
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
                    js_gen_string(prop.key),
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
            format!("({}) => {{\n {} }}", comma_separated_args, body_str)
        }
        JSAstNode::ArrayLiteral(nodes) => {
            let mut node_strs: Vec<String> = vec![];
            for node in nodes {
                node_strs.push(js_gen_string(node));
            }

            let comma_separated_node_strs = node_strs.join(", ");
            format!("[{}]", comma_separated_node_strs)
        }
        JSAstNode::StringLiteral(s) => format!("\"{}\"", s),
        JSAstNode::PlusExpr { left, right } => {
            format!("{} + {}", js_gen_string(*left), js_gen_string(*right))
        }
        JSAstNode::EqualityExpr { left, right } => {
            format!("{} === {}", js_gen_string(*left), js_gen_string(*right))
        }
        JSAstNode::NotEqualExpr { left, right } => {
            format!("{} !== {}", js_gen_string(*left), js_gen_string(*right))
        }
        /*JSAstNode::Expr(e) => e,*/
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
            typed_identifier: Box::new(js_translate(*typed_identifier)),
        },
        AstNode::TypedIdentifier { identifier, r#type } => JSAstNode::TypedIdentifier {
            identifier: Box::new(js_translate(*identifier)),
            r#type: Box::new(js_translate(*r#type)),
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
    state_variables: Vec<JSAstNode>,
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
    let mut state_variables: Vec<JSAstNode> = vec![];
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
                                        state_variables.push(def);
                                    }
                                }
                                _ => state_variables.push(def),
                            }
                        }
                        JSAstNode::ClassProperty { .. } => state_variables.push(def),
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
        state_variables: state_variables,
    }
}

fn js_class_method_name_expand(method_name: JSAstNode) -> JSAstNode {
    match method_name {
        JSAstNode::Identifier(n) => JSAstNode::Identifier(format!("{}Client", n)),
        _ => JSAstNode::InvalidNode,
    }
}

fn js_state_var_endpoint_client(
    state_var: &String,
    state_transition: &StateTransitionFunc,
    args: &Vec<String>,
) -> JSAstNode {
    match state_transition {
        StateTransitionFunc::Create | StateTransitionFunc::Read => {
            JSAstNode::StringLiteral(format!("{}/{}", API_HOST, state_var))
        }
        StateTransitionFunc::Delete | StateTransitionFunc::Update => {
            let state_var_arg = args[0].clone();
            JSAstNode::PlusExpr {
                left: Box::new(JSAstNode::StringLiteral(format!(
                    "{}/{}/",
                    API_HOST, state_var
                ))),
                right: Box::new(JSAstNode::Identifier(format!("{}.id", state_var_arg))),
            }
        }
    }
}

fn js_state_var_endpoint_server(state_var: &str, st_func: &StateTransitionFunc) -> String {
    match st_func {
        StateTransitionFunc::Create | StateTransitionFunc::Read => format!("/{}", state_var),
        StateTransitionFunc::Delete | StateTransitionFunc::Update => format!("/{}/:id", state_var),
    }
}

enum StateTransitionFunc {
    Create,
    Read,
    Update,
    Delete,
}

impl StateTransitionFunc {
    fn as_http_method(&self) -> &'static str {
        match self {
            StateTransitionFunc::Create => "POST",
            StateTransitionFunc::Read => "GET",
            StateTransitionFunc::Update => "PUT",
            StateTransitionFunc::Delete => "DELETE",
        }
    }

    fn as_express_http_method(&self) -> &'static str {
        match self {
            StateTransitionFunc::Create => "post",
            StateTransitionFunc::Read => "get",
            StateTransitionFunc::Update => "put",
            StateTransitionFunc::Delete => "delete",
        }
    }
}

fn state_transition_func_from_str(s: &str) -> StateTransitionFunc {
    match s {
        "create!" => StateTransitionFunc::Create,
        "read!" => StateTransitionFunc::Read,
        "update!" => StateTransitionFunc::Update,
        "delete!" => StateTransitionFunc::Delete,
        _ => panic!("Unexpected StateTransitionFunc string"),
    }
}

fn js_expand_fetch_args_from_state_transition(
    st: &StateTransitionFunc,
    args: &Vec<String>,
) -> JSAstNode {
    let method_prop = Prop {
        key: JSAstNode::Identifier("method".to_string()),
        value: JSAstNode::StringLiteral(st.as_http_method().to_string()),
    };

    let headers_prop = Prop {
        key: JSAstNode::Identifier("headers".to_string()),
        value: JSAstNode::Object {
            props: vec![Prop {
                key: JSAstNode::StringLiteral("Content-Type".to_string()),
                value: JSAstNode::StringLiteral("application/json".to_string()),
            }],
        },
    };

    let mut props: Vec<Prop> = vec![method_prop, headers_prop];

    match st {
        StateTransitionFunc::Create => {
            let state_var_iden = JSAstNode::Identifier(args[0].clone());
            let body_prop = Prop {
                key: JSAstNode::Identifier("body".to_string()),

                // TODO: Only JSON.stringifying one method call argument here
                value: JSAstNode::CallExpr {
                    receiver: Box::new(JSAstNode::Identifier("JSON".to_string())),
                    call_name: Box::new(JSAstNode::Identifier("stringify".to_string())),
                    args: vec![state_var_iden.clone()],
                },
            };
            props.push(body_prop);
        }
        _ => (),
    }

    JSAstNode::Object { props: props }
}

// This turns a semantic state transition into a network request to update the state in
// the database as well as optimistically update the client state
fn js_expand_state_transition_client(
    call_name: String,
    state_var: String,
    args: Vec<String>,
) -> JSAstNode {
    let state_trans_func = state_transition_func_from_str(&call_name);
    let endpoint = js_state_var_endpoint_client(&state_var, &state_trans_func, &args);
    let st_fetch_args = js_expand_fetch_args_from_state_transition(&state_trans_func, &args);

    let fetch_args = vec![endpoint, st_fetch_args];

    let fetch = JSAstNode::FuncCallExpr {
        call_name: Box::new(JSAstNode::Identifier("fetch".to_string())),
        args: fetch_args,
    };
    let expanded_statements: Vec<JSAstNode> = match state_trans_func {
        StateTransitionFunc::Create => {
            let state_var_iden = JSAstNode::Identifier(args[0].clone());
            let update_client_state = JSAstNode::CallExpr {
                receiver: Box::new(JSAstNode::Identifier(format!("this.{}", state_var))),
                call_name: Box::new(JSAstNode::Identifier("push".to_string())),
                args: vec![state_var_iden.clone()],
            };
            vec![fetch, update_client_state]
        }
        StateTransitionFunc::Delete => {
            let state_var_iden_str = args[0].clone();
            let update_client_state = JSAstNode::AssignmentStatement {
                left: Box::new(JSAstNode::Identifier(format!("this.{}", state_var))),
                right: Box::new(JSAstNode::CallExpr {
                    receiver: Box::new(JSAstNode::Identifier(format!("this.{}", state_var))),
                    call_name: Box::new(JSAstNode::Identifier("filter".to_string())),
                    args: vec![JSAstNode::ArrowClosure {
                        args: vec![JSAstNode::Identifier("data".to_string())],
                        body: Box::new(JSAstNode::ReturnStatement(Box::new(
                            JSAstNode::NotEqualExpr {
                                left: Box::new(JSAstNode::Identifier("data.id".to_string())),
                                right: Box::new(JSAstNode::Identifier(format!(
                                    "{}.id",
                                    state_var_iden_str
                                ))),
                            },
                        ))),
                    }],
                }),
            };
            vec![fetch, update_client_state]
        }
        StateTransitionFunc::Read => {
            /*
            let rts = await fetch("http://localhost:3000/recurring_transactions", { method: "GET", headers: { "Content-Type": "application/json" } });
            let rts_json = await rts.json();
            this.recurring_transactions = rts_json;
            */
            let await_fetch = JSAstNode::LetExpr {
                name: Box::new(JSAstNode::Identifier("data".to_string())),
                value: Box::new(JSAstNode::AwaitOperator {
                    node: Box::new(fetch),
                }),
            };
            let update_client_state = JSAstNode::AssignmentStatement {
                left: Box::new(JSAstNode::Identifier(format!("this.{}", state_var))),
                right: Box::new(JSAstNode::AwaitOperator {
                    node: Box::new(JSAstNode::CallExpr {
                        call_name: Box::new(JSAstNode::Identifier("json".to_string())),
                        receiver: Box::new(JSAstNode::Identifier("data".to_string())),
                        args: vec![],
                    }),
                }),
            };
            vec![await_fetch, update_client_state]
        }
        _ => vec![],
    };

    JSAstNode::StatementList {
        statements: expanded_statements,
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
    for cm in &class_defs.state_variables {
        expanded_definitions.push(cm.clone());
    }

    expanded_definitions.sort_by(js_ast_node_cmp);

    // This may not belong here - but here is where the constructor for the top-level
    // client state object is.
    let config_func_type = format!("(a: {}) => void", class_name);
    let constructor = JSAstNode::ClassMethod {
        name: Box::new(JSAstNode::Identifier("constructor".to_string())),
        args: vec![JSAstNode::TypedIdentifier {
            identifier: Box::new(JSAstNode::Identifier("config".to_string())),
            r#type: Box::new(JSAstNode::Identifier(config_func_type)),
        }],
        body: Box::new(JSAstNode::FuncCallExpr {
            args: vec![JSAstNode::Identifier("this".to_string())],
            call_name: Box::new(JSAstNode::Identifier("config".to_string())),
        }),
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
fn js_make_server(class_defs: &PartitionedClassDefinitions, schemas: &Schemas) -> Vec<JSAstNode> {
    let mut endpoints: Vec<JSAstNode> = vec![];
    for st in &class_defs.state_transitions {
        match st {
            JSAstNode::ClassMethod { body, args, .. } => {
                endpoints.push(js_expand_class_method_to_endpoint(
                    *body.clone(),
                    &args,
                    schemas,
                ));
            }
            _ => continue,
        }
    }

    endpoints
}

fn js_state_query_read(state_var: &str) -> JSAstNode {
    let sql = SQLAstNode::Select {
        from: Box::new(SQLAstNode::Identifier(state_var.to_string())),
        attributes: vec![SQLAstNode::Identifier("*".to_string())],
        clause: None,
    };
    JSAstNode::CallExpr {
        receiver: Box::new(JSAstNode::Identifier("db".to_string())),
        call_name: Box::new(JSAstNode::Identifier("all".to_string())),
        args: vec![
            JSAstNode::StringLiteral(sql_gen_string(&sql)),
            JSAstNode::ArrowClosure {
                args: vec![
                    JSAstNode::Identifier("_".to_string()),
                    JSAstNode::Identifier("rows".to_string()),
                ],
                body: Box::new(JSAstNode::CallExpr {
                    receiver: Box::new(JSAstNode::Identifier("res".to_string())),
                    call_name: Box::new(JSAstNode::Identifier("send".to_string())),
                    args: vec![JSAstNode::Identifier("rows".to_string())],
                }),
            },
        ],
    }
}

fn js_state_query_create(state_var: &str, state_var_type: &str, schemas: &Schemas) -> JSAstNode {
    let mut attr_names: Vec<String> = vec![];
    let mut sql_attr_names: Vec<SQLAstNode> = vec![];
    let mut sql_value_placeholders: Vec<SQLAstNode> = vec![];
    let mut js_attr_values: Vec<JSAstNode> = vec![];
    let schema = &schemas[state_var_type];
    for attr in &schema.attributes {
        attr_names.push(attr.name.clone());

        // TODO: this is assuming the name of 'data' which is used to
        // parse the HTTP body in write requests.
        js_attr_values.push(JSAstNode::Identifier(format!("data.{}", attr.name)));
        sql_attr_names.push(SQLAstNode::Identifier(attr.name.clone()));
        sql_value_placeholders.push(SQLAstNode::Identifier("?".to_string()))
    }
    // INSERT INTO state_var (attr_names) VALUES (?, ?, ?)
    let sql = SQLAstNode::Insert {
        into: Box::new(SQLAstNode::Identifier(state_var.to_string())),
        attributes: sql_attr_names,
        values: sql_value_placeholders,
    };
    JSAstNode::CallExpr {
        receiver: Box::new(JSAstNode::Identifier("db".to_string())),
        call_name: Box::new(JSAstNode::Identifier("run".to_string())),
        args: vec![
            JSAstNode::StringLiteral(sql_gen_string(&sql)),
            JSAstNode::ArrayLiteral(js_attr_values),
        ],
    }
}

fn js_state_query_delete(state_var: &str) -> JSAstNode {
    // DELETE FROM state_var
    let sql = SQLAstNode::Delete {
        from: Box::new(SQLAstNode::Identifier(state_var.to_string())),
        clause: Some(Box::new(SQLAstNode::WhereClause {
            predicate: Box::new(SQLAstNode::EqualityExpr {
                left: Box::new(SQLAstNode::Identifier("id".to_string())),
                right: Box::new(SQLAstNode::Identifier("?".to_string())),
            }),
        })),
    };
    let js_attr_values: Vec<JSAstNode> = vec![JSAstNode::Identifier("req.params.id".to_string())];
    JSAstNode::CallExpr {
        receiver: Box::new(JSAstNode::Identifier("db".to_string())),
        call_name: Box::new(JSAstNode::Identifier("run".to_string())),
        args: vec![
            JSAstNode::StringLiteral(sql_gen_string(&sql)),
            JSAstNode::ArrayLiteral(js_attr_values),
        ],
    }
}

// Expand state transitions into SQL queries inside of server
fn js_expand_class_method_to_endpoint(
    body: JSAstNode,
    args: &Vec<JSAstNode>,
    schemas: &Schemas,
) -> JSAstNode {
    match body {
        JSAstNode::CallExpr {
            receiver,
            call_name,
            ..
        } => {
            let state_var = js_gen_iden_name(*receiver);
            let call_name_str = js_gen_iden_name(*call_name);
            let state_transition_func = state_transition_func_from_str(&call_name_str);
            let express_method = state_transition_func.as_express_http_method();
            let endpoint_path = js_state_var_endpoint_server(&state_var, &state_transition_func);

            // TODO: For state transitions like Create, need a "request" type that does not
            // have an id so data can be passed to such methods before creation
            let state_body = match state_transition_func {
                StateTransitionFunc::Create => {
                    // Again - only handling one method arg here.
                    // also need to switch on StateTransitionFunc here.
                    let state_var_type = match &args[0] {
                        JSAstNode::TypedIdentifier { r#type, .. } => {
                            js_gen_iden_name(*r#type.clone())
                        }
                        _ => panic!(
                            "Expected a TypedIdentifier to be the first argument of a class method"
                        ),
                    };
                    let query = js_state_query_create(&state_var, &state_var_type, schemas);
                    let parse_data = JSAstNode::LetExpr {
                        name: Box::new(JSAstNode::Identifier("data".to_string())),
                        value: Box::new(JSAstNode::Identifier("req.body".to_string())),
                    };
                    Box::new(JSAstNode::StatementList {
                        statements: vec![
                            parse_data,
                            query,
                            JSAstNode::CallExpr {
                                receiver: Box::new(JSAstNode::Identifier("res".to_string())),
                                call_name: Box::new(JSAstNode::Identifier("send".to_string())),
                                args: vec![JSAstNode::Object { props: vec![] }],
                            },
                        ],
                    })
                }
                StateTransitionFunc::Read => {
                    let query = js_state_query_read(&state_var);
                    Box::new(query)
                }
                StateTransitionFunc::Delete => {
                    let query = js_state_query_delete(&state_var);
                    Box::new(JSAstNode::StatementList {
                        statements: vec![
                            query,
                            JSAstNode::CallExpr {
                                receiver: Box::new(JSAstNode::Identifier("res".to_string())),
                                call_name: Box::new(JSAstNode::Identifier("send".to_string())),
                                args: vec![JSAstNode::Object { props: vec![] }],
                            },
                        ],
                    })
                }
                _ => Box::new(JSAstNode::CallExpr {
                    receiver: Box::new(JSAstNode::Identifier("res".to_string())),
                    call_name: Box::new(JSAstNode::Identifier("send".to_string())),
                    args: vec![JSAstNode::Object { props: vec![] }],
                }),
            };

            let endpoint_body = JSAstNode::ArrowClosure {
                args: vec![
                    JSAstNode::Identifier("req".to_string()),
                    JSAstNode::Identifier("res".to_string()),
                ],
                body: state_body,
            };

            JSAstNode::CallExpr {
                receiver: Box::new(JSAstNode::Identifier("app".to_string())),
                call_name: Box::new(JSAstNode::Identifier(express_method.to_string())),
                args: vec![JSAstNode::StringLiteral(endpoint_path), endpoint_body],
            }
        }
        _ => panic!("Unexpected JSAstNode type, should only be expanding CallExprs"),
    }
}

// might want to write quoted JS macros here:
// consider doing macros in MyLang, i.e. write infra in
// MyLang first before translating to target lang ?. Every
// func call / symbol in MyLang would have to be translated
// by the backend. I.e. client.request() maps to fetch in JS.
fn js_infra_expand(node: JSAstNode, schemas: &Schemas) -> (JSAstNode, Vec<JSAstNode>) {
    match node {
        JSAstNode::ClassDef { ref name, .. } => {
            let partitioned_class_defs = js_partition_class_definitions(node.clone());
            // TODO: I don't know why &** is necessary here
            let class_name = match &**name {
                JSAstNode::Identifier(n) => n,
                _ => panic!("Expected identifier"),
            };
            let client = js_make_client(class_name.clone(), &partitioned_class_defs);
            let server = js_make_server(&partitioned_class_defs, schemas);

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

// SQL

enum SQLAstNode {
    Insert {
        into: Box<SQLAstNode>,
        attributes: Vec<SQLAstNode>,
        values: Vec<SQLAstNode>,
    },
    Identifier(String),
    Select {
        attributes: Vec<SQLAstNode>,
        from: Box<SQLAstNode>,
        clause: Option<Box<SQLAstNode>>,
    },
    Delete {
        from: Box<SQLAstNode>,
        clause: Option<Box<SQLAstNode>>,
    },
    WhereClause {
        predicate: Box<SQLAstNode>, // Example: EqualityExpr
    },
    EqualityExpr {
        left: Box<SQLAstNode>,
        right: Box<SQLAstNode>,
    },
    /*
    Update {
        from: Box<SQLAstNode>,
        attributes: Vec<SQLAstNode>, // Vec<EqualityExpr>
        clause: Option<Box<SQLAstNode>>
    },

    StringLiteral(String),
    NumberLiteral(i32),
    */
}

fn sql_gen_string(node: &SQLAstNode) -> String {
    match node {
        SQLAstNode::Insert {
            into,
            attributes,
            values,
        } => {
            let into_relation = sql_gen_string(into);
            let mut attr_names: Vec<String> = vec![];
            for attr in attributes {
                attr_names.push(sql_gen_string(attr));
            }

            let mut attr_values: Vec<String> = vec![];
            for value in values {
                attr_values.push(sql_gen_string(value));
            }

            let comma_separated_attrs = attr_names.join(", ");
            let comma_separated_values = attr_values.join(", ");

            format!(
                "INSERT INTO {} ({}) VALUES ({})",
                into_relation, comma_separated_attrs, comma_separated_values
            )
        }
        SQLAstNode::Select {
            from, attributes, ..
        } => {
            let from_relation = sql_gen_string(from);
            let mut attr_names: Vec<String> = vec![];
            for attr in attributes {
                attr_names.push(sql_gen_string(attr));
            }
            let comma_separated_attrs = attr_names.join(", ");

            if comma_separated_attrs.len() == 1 {
                format!("SELECT {} FROM {}", comma_separated_attrs, from_relation)
            } else {
                format!("SELECT ({}) FROM {}", comma_separated_attrs, from_relation)
            }
        }
        SQLAstNode::Delete { from, clause } => match clause {
            Some(c) => format!("DELETE FROM {} {}", sql_gen_string(from), sql_gen_string(c)),
            None => format!("DELETE FROM {}", sql_gen_string(from)),
        },
        SQLAstNode::WhereClause { predicate } => format!("WHERE {}", sql_gen_string(predicate)),
        SQLAstNode::EqualityExpr { left, right } => {
            format!("{} = {}", sql_gen_string(left), sql_gen_string(right))
        }
        SQLAstNode::Identifier(n) => n.clone(),
    }
}

type Schemas = HashMap<String, Schema>;

struct SchemaAttribute {
    name: String,
    // r#type: String
}

struct Schema {
    /* name: String, */
    attributes: Vec<SchemaAttribute>,
}

fn iden_name(iden: AstNode) -> String {
    match iden {
        AstNode::Identifier(s) => s,
        _ => panic!("This is for extracting names out of Identifiers only"),
    }
}

fn schema_attributes(schema_body: AstNode) -> Vec<SchemaAttribute> {
    let mut attrs: Vec<SchemaAttribute> = vec![];
    match schema_body {
        AstNode::SchemaBody { definitions } => {
            for def in definitions {
                match def {
                    AstNode::SchemaAttribute { typed_identifier } => match *typed_identifier {
                        AstNode::TypedIdentifier { identifier, .. /*, r#type*/ } => {
                            let attr_name = iden_name(*identifier);
                            // let attr_type = iden_name(*r#type);

                            attrs.push(SchemaAttribute { name: attr_name /*, r#type: attr_type*/ });
                        },
                        _ => continue
                    },
                    _ => continue,
                }
            }

            attrs
        }
        _ => panic!("Can only extract schema attributes out of SchemaBodies"),
    }
}

fn main() {
    let source_file = std::env::args().nth(1).expect("No source file specified");
    let source = std::fs::read_to_string(source_file).expect("Gotta exist");
    let result = LangParser::parse(Rule::Program, &source);
    let mut schemas: Schemas = HashMap::new();
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
                match parsed.clone() {
                    AstNode::SchemaDef { name, body } => {
                        let schema_name = iden_name(*name);
                        let attributes = schema_attributes(*body);
                        let schema = Schema {
                            /*name: schema_name.clone(),*/
                            attributes: attributes,
                        };

                        schemas.insert(schema_name, schema);
                    }
                    _ => (),
                }

                statements.push(parsed.clone());
                let js_ast = js_translate(parsed.clone());
                js_asts.push(js_ast.clone());
                js_code.push(js_gen_string(js_ast.clone()));
                let (client, server) = js_infra_expand(js_ast, &schemas);
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

    println!("JS Translation:\n");
    for c in js_code {
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

    for js_ast in js_asts {
        println!("JSAst:");
        println!("{:?}\n\n", js_ast);
    }
}
