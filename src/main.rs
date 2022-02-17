use clap::Parser as ClapParser;
use pest::{self, Parser as PestParser};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fs;

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
    ExprList(Vec<AstNode>),
    LetExpr {
        name: Box<AstNode>, // Identifier
    },
    FuncCall {
        name: Box<AstNode>,
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
    FuncDef {
        name: Box<JSAstNode>,
        args: Vec<JSAstNode>,
        body: Box<JSAstNode>,
        return_type: Box<JSAstNode>,
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
    NewClass {
        name: Box<JSAstNode>,
        args: Vec<JSAstNode>,
    },
    ArrowClosure {
        args: Vec<JSAstNode>,
        body: Box<JSAstNode>,
    },
    StatementList {
        statements: Vec<JSAstNode>,
    },
    ReturnStatement(Box<JSAstNode>),
    AsyncModifier(Box<JSAstNode>), // ClassMethod, ArrowClosure
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
        JSAstNode::AsyncModifier(node) => {
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
        JSAstNode::FuncDef {
            name, args, body, ..
        } => {
            let name_str = js_gen_string(*name);
            let mut arg_strs: Vec<String> = vec![];
            for arg in args {
                arg_strs.push(js_gen_string(arg));
            }
            let comma_separated_args = arg_strs.join(", ");
            let body_str = js_gen_string(*body);

            format!(
                "function {}({}) {{\n  {}\n}}",
                name_str, comma_separated_args, body_str
            )
        }
        JSAstNode::CallExpr {
            receiver,
            call_name,
            args,
        } => {
            let receiver_name = js_gen_string(*receiver);

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
        JSAstNode::NewClass { name, args } => {
            let mut arg_strs: Vec<String> = vec![];
            for arg in args {
                arg_strs.push(js_gen_string(arg));
            }
            let comma_separated_args = arg_strs.join(", ");
            format!("new {}({})", js_gen_string(*name), comma_separated_args)
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

fn js_translate_to_class_method(name: AstNode, args: Vec<AstNode>, body: AstNode) -> JSAstNode {
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

// A better name for this might be js_translate_initial
// or something like that.
// it represents a bridge between the source lang
// and JS, but it still has some syntax rules of
// the source, i.e. 'create!' is a valid identifier
// in the source, but not in JS.
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
        } => {
            let mut js_args: Vec<JSAstNode> = vec![];
            for arg in args {
                js_args.push(js_translate(arg));
            }

            JSAstNode::FuncDef {
                name: Box::new(js_translate(*name)),
                args: js_args,
                body: Box::new(js_translate(*body)),
                return_type: Box::new(JSAstNode::InvalidNode),
            }
        }
        AstNode::SchemaBody { definitions } => {
            let mut js_definitions: Vec<JSAstNode> = vec![];
            for def in definitions {
                match def {
                    AstNode::SchemaMethod {
                        name, args, body, ..
                    } => js_definitions.push(js_translate_to_class_method(*name, args, *body)),
                    _ => js_definitions.push(js_translate(def)),
                }
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
        AstNode::ExprList(exprs) => {
            let mut js_exprs: Vec<JSAstNode> = vec![];
            for expr in exprs {
                js_exprs.push(js_translate(expr));
            }

            JSAstNode::StatementList {
                statements: js_exprs,
            }
        }
        _ => JSAstNode::InvalidNode,
    }
}

// Infrastructure expansion

struct PartitionedClassDefinitions {
    state_transitions: Vec<StateTransition>,
    state_transition_nodes: Vec<JSAstNode>,
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

struct TypedArgument {
    name: String,
    r#type: String,
}

struct StateTransition {
    name: String,
    state_variable: String,
    transition_type: StateTransitionFunc,
    args: Vec<TypedArgument>,
    node: JSAstNode,
}

fn is_state_transition(func_name: &str) -> bool {
    let name_chars: Vec<char> = func_name.chars().collect();

    *name_chars.last().unwrap() == '!'
}

fn js_partition_class_definitions(node: &JSAstNode) -> PartitionedClassDefinitions {
    let mut state_transitions: Vec<StateTransition> = vec![];
    let mut state_transition_nodes: Vec<JSAstNode> = vec![];
    let mut state_variables: Vec<JSAstNode> = vec![];
    match node {
        JSAstNode::ClassDef { body, .. } => match &**body {
            JSAstNode::ClassBody { definitions } => {
                for def in definitions {
                    match def.clone() {
                        JSAstNode::ClassMethod {
                            body,
                            name,
                            args: method_def_args,
                        } => match *body {
                            JSAstNode::StatementList { statements } => {
                                for statement in &statements {
                                    match statement {
                                        JSAstNode::CallExpr {
                                            call_name,
                                            receiver,
                                            ..
                                        } => {
                                            let method_name = js_gen_iden_name(*name.clone());
                                            let receiver_name = js_gen_iden_name(*receiver.clone());
                                            let state_transition_name =
                                                js_gen_iden_name(*call_name.clone());
                                            if is_state_transition(&state_transition_name) {
                                                let mut state_trans_args: Vec<TypedArgument> =
                                                    vec![];
                                                for arg in method_def_args {
                                                    match arg {
                                                        JSAstNode::TypedIdentifier {
                                                            identifier,
                                                            r#type,
                                                        } => state_trans_args.push(TypedArgument {
                                                            name: js_gen_iden_name(*identifier),
                                                            r#type: js_gen_iden_name(*r#type),
                                                        }),
                                                        _ => continue,
                                                    }
                                                }
                                                state_transitions.push(StateTransition {
                                                    name: method_name,
                                                    state_variable: receiver_name,
                                                    args: state_trans_args,
                                                    transition_type: state_transition_func_from_str(
                                                        &state_transition_name,
                                                    ),
                                                    node: def.clone(),
                                                });
                                                state_transition_nodes.push(def.clone());
                                                break;
                                            } else {
                                                state_variables.push(def.clone());
                                            }
                                        }
                                        _ => state_variables.push(def.clone()),
                                    }
                                }
                            }
                            _ => state_variables.push(def.clone()),
                        },
                        JSAstNode::ClassProperty { .. } => state_variables.push(def.clone()),
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
        state_transition_nodes: state_transition_nodes,
        state_variables: state_variables,
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

    // Note: Read is considered a state transition because it queries the
    // server state and applies it to the client state. It isn't a _read_ of the state,
    // aka state function, but a transition where the client state is updated
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

fn js_push_var(state_var: &str, state_var_val: &str) -> JSAstNode {
    JSAstNode::CallExpr {
        receiver: Box::new(JSAstNode::Identifier(format!("this.{}", state_var))),
        call_name: Box::new(JSAstNode::Identifier("push".to_string())),
        args: vec![JSAstNode::Identifier(state_var_val.to_string())],
    }
}

fn js_delete_var(state_var: &str, state_var_val: &str) -> JSAstNode {
    JSAstNode::AssignmentStatement {
        left: Box::new(JSAstNode::Identifier(format!("this.{}", state_var))),
        right: Box::new(JSAstNode::CallExpr {
            receiver: Box::new(JSAstNode::Identifier(format!("this.{}", state_var))),
            call_name: Box::new(JSAstNode::Identifier("filter".to_string())),
            args: vec![JSAstNode::ArrowClosure {
                args: vec![JSAstNode::Identifier("data".to_string())],
                body: Box::new(JSAstNode::ReturnStatement(Box::new(
                    JSAstNode::NotEqualExpr {
                        left: Box::new(JSAstNode::Identifier("data.id".to_string())),
                        right: Box::new(JSAstNode::Identifier(format!("{}.id", state_var_val))),
                    },
                ))),
            }],
        }),
    }
}

fn js_read_var(state_var: &str) -> JSAstNode {
    JSAstNode::AssignmentStatement {
        left: Box::new(JSAstNode::Identifier(format!("this.{}", state_var))),
        right: Box::new(JSAstNode::AwaitOperator {
            node: Box::new(JSAstNode::CallExpr {
                call_name: Box::new(JSAstNode::Identifier("json".to_string())),
                receiver: Box::new(JSAstNode::Identifier("data".to_string())),
                args: vec![],
            }),
        }),
    }
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
            let await_fetch = JSAstNode::LetExpr {
                name: Box::new(JSAstNode::Identifier("resp".to_string())),
                value: Box::new(JSAstNode::AwaitOperator {
                    node: Box::new(fetch),
                }),
            };
            let await_json = JSAstNode::AwaitOperator {
                node: Box::new(JSAstNode::CallExpr {
                    receiver: Box::new(JSAstNode::Identifier("resp".to_string())),
                    call_name: Box::new(JSAstNode::Identifier("json".to_string())),
                    args: vec![],
                }),
            };
            let update_client_state = JSAstNode::CallExpr {
                receiver: Box::new(JSAstNode::Identifier(format!("this.{}", state_var))),
                call_name: Box::new(JSAstNode::Identifier("push".to_string())),
                args: vec![await_json],
            };
            vec![await_fetch, update_client_state]
        }
        StateTransitionFunc::Delete => {
            let update_client_state = js_delete_var(&state_var, &args[0]);
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
            let update_client_state = js_read_var(&state_var);
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
        JSAstNode::ClassMethod { name, args, body } => match *body {
            JSAstNode::StatementList { statements } => {
                let mut expanded_statements: Vec<JSAstNode> = vec![];
                for statement in statements {
                    expanded_statements.push(js_class_method_body_expand(statement.clone()));
                }

                JSAstNode::ClassMethod {
                    name: name,
                    args: args,
                    body: Box::new(JSAstNode::StatementList {
                        statements: expanded_statements,
                    }),
                }
            }
            _ => panic!("Attempted to client-expand "),
        },
        _ => JSAstNode::InvalidNode,
    }
}

fn js_make_client(class_name: String, class_defs: &PartitionedClassDefinitions) -> JSAstNode {
    let mut expanded_definitions: Vec<JSAstNode> = vec![];
    for st in &class_defs.state_transition_nodes {
        expanded_definitions.push(js_expand_client(st.clone()))
    }
    for cm in &class_defs.state_variables {
        expanded_definitions.push(cm.clone());
    }

    expanded_definitions.sort_by(js_ast_node_cmp);

    //
    let mut defs_with_async_state_transitions: Vec<JSAstNode> = vec![];
    for def in expanded_definitions {
        match def {
            JSAstNode::ClassMethod { .. } => {
                defs_with_async_state_transitions.push(JSAstNode::AsyncModifier(Box::new(def)))
            }
            _ => defs_with_async_state_transitions.push(def),
        }
    }

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

    defs_with_async_state_transitions.insert(0, constructor);

    // Quoted macro version:
    // quote: class Client {
    //   `expanded_definitions`
    // }
    JSAstNode::ClassDef {
        name: Box::new(JSAstNode::Identifier(class_name)),
        body: Box::new(JSAstNode::ClassBody {
            definitions: defs_with_async_state_transitions,
        }),
    }
}

// We generate a set of endpoints corresponding to all each state transition
fn js_make_server(class_defs: &PartitionedClassDefinitions, schemas: &Schemas) -> Vec<JSAstNode> {
    let mut endpoints: Vec<JSAstNode> = vec![];
    for st in &class_defs.state_transition_nodes {
        match st {
            JSAstNode::ClassMethod { body, args, .. } => match &**body {
                JSAstNode::StatementList { statements } => {
                    for statement in statements {
                        match statement {
                            JSAstNode::CallExpr { .. } => {
                                endpoints.push(js_expand_state_transition_to_endpoint(
                                    statement.clone(),
                                    &args,
                                    schemas,
                                ));
                            }
                            _ => continue,
                        }
                    }
                }
                _ => continue,
            },
            _ => continue,
        }
    }

    endpoints
}

fn js_state_query_read(state_var: &str) -> JSAstNode {
    let sql = SQLAstNode::Select {
        from: Some(Box::new(SQLAstNode::Identifier(state_var.to_string()))),
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
    let mut response_props: Vec<Prop> = vec![];
    let schema = &schemas[state_var_type];
    for attr in &schema.attributes {
        attr_names.push(attr.name.clone());

        // TODO: this is assuming the name of 'data' which is used to
        // parse the HTTP body in write requests.
        js_attr_values.push(JSAstNode::Identifier(format!("data.{}", attr.name)));
        response_props.push(Prop {
            key: JSAstNode::Identifier(attr.name.clone()),
            value: JSAstNode::Identifier(format!("data.{}", attr.name)),
        });
        sql_attr_names.push(SQLAstNode::Identifier(attr.name.clone()));
        sql_value_placeholders.push(SQLAstNode::Identifier("?".to_string()))
    }
    /*
    db.serialize(() => {
        db.run("INSERT INTO recurring_transactions (amount, name) VALUES (?, ?)", [data.amount, data.name]);
        db.get("SELECT last_insert_rowid()", (err, row) => {
          res.send({...data, id: row});
        });
      });
      */
    let insert_sql = SQLAstNode::Insert {
        into: Box::new(SQLAstNode::Identifier(state_var.to_string())),
        attributes: sql_attr_names,
        values: sql_value_placeholders,
    };
    let insert_js = JSAstNode::CallExpr {
        receiver: Box::new(JSAstNode::Identifier("db".to_string())),
        call_name: Box::new(JSAstNode::Identifier("run".to_string())),
        args: vec![
            JSAstNode::StringLiteral(sql_gen_string(&insert_sql)),
            JSAstNode::ArrayLiteral(js_attr_values),
        ],
    };
    let get_id_sql = SQLAstNode::Select {
        attributes: vec![SQLAstNode::Identifier("last_insert_rowid()".to_string())],
        from: None,
        clause: None,
    };

    response_props.push(Prop {
        key: JSAstNode::Identifier("id".to_string()),
        value: JSAstNode::Identifier("row[\"last_insert_rowid()\"]".to_string()),
    });
    let respond = JSAstNode::CallExpr {
        receiver: Box::new(JSAstNode::Identifier("res".to_string())),
        call_name: Box::new(JSAstNode::Identifier("send".to_string())),
        args: vec![JSAstNode::Object {
            props: response_props,
        }],
    };

    let respond_with_id = JSAstNode::CallExpr {
        receiver: Box::new(JSAstNode::Identifier("db".to_string())),
        call_name: Box::new(JSAstNode::Identifier("get".to_string())),
        args: vec![
            JSAstNode::StringLiteral(sql_gen_string(&get_id_sql)),
            JSAstNode::ArrowClosure {
                args: vec![
                    JSAstNode::Identifier("err".to_string()),
                    JSAstNode::Identifier("row".to_string()),
                ],
                body: Box::new(respond),
            },
        ],
    };

    JSAstNode::CallExpr {
        receiver: Box::new(JSAstNode::Identifier("db".to_string())),
        call_name: Box::new(JSAstNode::Identifier("serialize".to_string())),
        args: vec![JSAstNode::ArrowClosure {
            args: vec![],
            body: Box::new(JSAstNode::StatementList {
                statements: vec![insert_js, respond_with_id],
            }),
        }],
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
fn js_expand_state_transition_to_endpoint(
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
                        statements: vec![parse_data, query],
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
fn js_infra_expand(
    node: JSAstNode,
    schemas: &Schemas,
    partitioned_class_defs: &PartitionedClassDefinitions,
) -> (JSAstNode, Vec<JSAstNode>) {
    match node {
        JSAstNode::ClassDef { ref name, .. } => {
            println!("State transitions: ");
            for trans in &partitioned_class_defs.state_transition_nodes {
                match trans {
                    JSAstNode::ClassMethod { name, .. } => {
                        println!("{}", js_gen_string(*name.clone()));
                    }
                    _ => continue,
                }
            }

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

// JS Executable Compilation

fn js_executable_translate(node: &JSAstNode) -> JSAstNode {
    match node {
        JSAstNode::ClassDef { name, body } => {
            let translated_name = js_executable_translate(&name);
            let translated_body = js_executable_translate(&body);

            JSAstNode::ClassDef {
                name: Box::new(translated_name),
                body: Box::new(translated_body),
            }
        }
        JSAstNode::ClassBody { definitions } => {
            let mut translated: Vec<JSAstNode> = vec![];
            for def in definitions {
                translated.push(js_executable_translate(&def));
            }

            JSAstNode::ClassBody {
                definitions: translated,
            }
        }
        JSAstNode::ClassMethod { name, args, body } => JSAstNode::ClassMethod {
            name: name.clone(),
            args: args.clone(),
            body: Box::new(js_executable_translate(body)),
        },
        JSAstNode::StatementList { statements } => {
            let mut executable_statements: Vec<JSAstNode> = vec![];
            for statement in statements {
                executable_statements.push(js_executable_translate(statement));
            }
            JSAstNode::StatementList {
                statements: executable_statements,
            }
        }
        JSAstNode::CallExpr {
            receiver,
            call_name,
            args,
        } => {
            let call_str = js_gen_string(*call_name.clone());
            let state_trans_func = state_transition_func_from_str(&call_str);
            let state_var = js_gen_string(*receiver.clone());
            match state_trans_func {
                StateTransitionFunc::Create => {
                    let state_var_val = js_gen_string(args[0].clone());
                    js_push_var(&state_var, &state_var_val)
                }
                StateTransitionFunc::Delete => {
                    let state_var_val = js_gen_string(args[0].clone());
                    js_delete_var(&state_var, &state_var_val)
                }
                _ => node.clone(),
            }
        }
        _ => node.clone(),
    }
}

// Parser

fn identifier(pair: pest::iterators::Pair<Rule>) -> AstNode {
    return AstNode::Identifier(pair.as_str().into());
}

fn schema_method(pair: pest::iterators::Pair<Rule>) -> AstNode {
    // Only handling methods with arguments right now
    // println!("Parsing schema method");
    // println!("{}", pair.to_json());

    let mut schema_method = pair.into_inner();

    let mut method_args = schema_method.next().unwrap().into_inner();
    let name = parse(method_args.next().unwrap());
    let args = method_args.map(parse).collect();

    let method_body = parse(schema_method.next().unwrap());
    //    let expr = method_body.map(parse).collect();

    return AstNode::SchemaMethod {
        name: Box::new(name),
        args: args,
        body: Box::new(method_body),
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
        Rule::ExprList => AstNode::ExprList(pair.into_inner().map(parse).collect()),
        Rule::LetExpr => AstNode::InvalidNode,
        Rule::FuncCall => AstNode::InvalidNode,
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
        from: Option<Box<SQLAstNode>>,
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
        } => match from {
            Some(f) => {
                let from_relation = sql_gen_string(f);
                let mut attr_names: Vec<String> = vec![];
                for attr in attributes {
                    attr_names.push(sql_gen_string(attr));
                }
                let comma_separated_attrs = attr_names.join(", ");

                if attributes.len() == 1 {
                    format!("SELECT {} FROM {}", comma_separated_attrs, from_relation)
                } else {
                    format!("SELECT ({}) FROM {}", comma_separated_attrs, from_relation)
                }
            }
            None => {
                let mut attr_names: Vec<String> = vec![];
                for attr in attributes {
                    attr_names.push(sql_gen_string(attr));
                }
                let comma_separated_attrs = attr_names.join(", ");

                if attributes.len() == 1 {
                    format!("SELECT {}", comma_separated_attrs)
                } else {
                    format!("SELECT ({})", comma_separated_attrs)
                }
            }
        },
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
    r#type: String,
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
                        AstNode::TypedIdentifier { identifier, r#type } => {
                            let attr_name = iden_name(*identifier);
                            let attr_type = iden_name(*r#type);

                            attrs.push(SchemaAttribute {
                                name: attr_name,
                                r#type: attr_type,
                            });
                        }
                        _ => continue,
                    },
                    _ => continue,
                }
            }

            attrs
        }
        _ => panic!("Can only extract schema attributes out of SchemaBodies"),
    }
}

// Translation Certification

fn fast_check_arbitrary_from_type(type_name: &str) -> String {
    match type_name {
        "String" => "string".to_string(),
        "Numeric" => "float".to_string(),
        _ => panic!("Trying to convert unknown type name to a fast-check arbitrary"),
    }
}

fn js_gen_certification_properties(
    partitioned_class_defs: &PartitionedClassDefinitions,
    schemas: &Schemas,
) -> (Vec<JSAstNode>, Vec<String>) {
    let mut js_property_defs: Vec<JSAstNode> = vec![];
    let mut js_property_names: Vec<String> = vec![];
    for state_transition in &partitioned_class_defs.state_transitions {
        let state_trans_name = &state_transition.name;
        let state_trans_func = &state_transition.transition_type;
        js_property_names.push(state_trans_name.clone());

        match state_trans_func {
            StateTransitionFunc::Create => {
                // Create data generators for all transition arguments
                let mut data_generators: Vec<JSAstNode> = vec![];
                // Argument list for property body
                let mut generated_data_vars: Vec<JSAstNode> = vec![];
                for arg in &state_transition.args {
                    println!("Looking for schema named  {}", arg.r#type);
                    let schema = &schemas[&arg.r#type];
                    let mut data_generator_props: Vec<Prop> = vec![];
                    for attr in &schema.attributes {
                        let key_node = JSAstNode::Identifier(attr.name.clone());
                        let value_generator = JSAstNode::CallExpr {
                            receiver: Box::new(JSAstNode::Identifier("fc".to_string())),
                            call_name: Box::new(JSAstNode::Identifier(
                                fast_check_arbitrary_from_type(&attr.r#type),
                            )),
                            args: vec![],
                        };
                        data_generator_props.push(Prop {
                            key: key_node,
                            value: value_generator,
                        })
                    }
                    data_generators.push(JSAstNode::Object {
                        props: data_generator_props,
                    });
                    generated_data_vars.push(JSAstNode::Identifier(arg.name.clone()));
                }

                let property_func_name = format!("{}Property", state_trans_name);

                let mut property_args: Vec<JSAstNode> = vec![];
                for data_generator in data_generators {
                    property_args.push(JSAstNode::CallExpr {
                        receiver: Box::new(JSAstNode::Identifier("fc".to_string())),
                        call_name: Box::new(JSAstNode::Identifier("record".to_string())),
                        args: vec![data_generator],
                    })
                }

                let test_body = JSAstNode::StatementList {
                    statements: vec![
                        JSAstNode::LetExpr {
                            name: Box::new(JSAstNode::Identifier("model".to_string())),
                            value: Box::new(JSAstNode::NewClass {
                                name: Box::new(JSAstNode::Identifier("Model".to_string())),
                                args: vec![],
                            }),
                        },
                        JSAstNode::LetExpr {
                            name: Box::new(JSAstNode::Identifier("fullstack".to_string())),
                            value: Box::new(JSAstNode::NewClass {
                                name: Box::new(JSAstNode::Identifier("Fullstack".to_string())),
                                args: vec![JSAstNode::ArrowClosure {
                                    args: vec![],
                                    body: Box::new(JSAstNode::StatementList { statements: vec![] }),
                                }],
                            }),
                        },
                        JSAstNode::LetExpr {
                            name: Box::new(JSAstNode::Identifier("created".to_string())),
                            value: Box::new(JSAstNode::AwaitOperator {
                                node: Box::new(JSAstNode::CallExpr {
                                    receiver: Box::new(JSAstNode::Identifier(
                                        "fullstack".to_string(),
                                    )),
                                    call_name: Box::new(JSAstNode::Identifier(
                                        state_trans_name.to_string(),
                                    )),
                                    // args[0] again - represents a state transition argument
                                    args: vec![JSAstNode::Identifier(
                                        state_transition.args[0].name.clone(),
                                    )],
                                }),
                            }),
                        },
                        JSAstNode::CallExpr {
                            receiver: Box::new(JSAstNode::Identifier("model".to_string())),
                            call_name: Box::new(JSAstNode::Identifier(
                                state_trans_name.to_string(),
                            )),
                            args: vec![
                                JSAstNode::Identifier(state_transition.args[0].name.clone()),
                                // Pass in id of created entity so that the model and implementation are creating
                                // the same entity.
                                JSAstNode::Identifier("created.id".to_string()),
                            ],
                        },
                        // expect(fullstack.recurring_transactions).to.deep.eq(model.recurring_transactions);
                        JSAstNode::CallExpr {
                            receiver: Box::new(JSAstNode::FuncCallExpr {
                                call_name: Box::new(JSAstNode::Identifier("expect".to_string())),
                                args: vec![JSAstNode::Identifier(format!(
                                    "fullstack.{}",
                                    state_transition.state_variable
                                ))],
                            }),
                            call_name: Box::new(JSAstNode::Identifier("to.deep.eq".to_string())),
                            args: vec![JSAstNode::Identifier(format!(
                                "model.{}",
                                state_transition.state_variable
                            ))],
                        },
                    ],
                };

                property_args.push(JSAstNode::AsyncModifier(Box::new(
                    JSAstNode::ArrowClosure {
                        args: generated_data_vars,
                        body: Box::new(test_body),
                    },
                )));
                let property_func = JSAstNode::FuncDef {
                    name: Box::new(JSAstNode::Identifier(property_func_name)),
                    body: Box::new(JSAstNode::StatementList {
                        statements: vec![JSAstNode::ReturnStatement(Box::new(
                            JSAstNode::CallExpr {
                                receiver: Box::new(JSAstNode::Identifier("fc".to_string())),
                                call_name: Box::new(JSAstNode::Identifier(
                                    "asyncProperty".to_string(),
                                )),
                                args: property_args,
                            },
                        ))],
                    }),
                    args: vec![],
                    return_type: Box::new(JSAstNode::InvalidNode),
                };

                js_property_defs.push(property_func)
            }
            _ => (),
        }
    }

    (js_property_defs, js_property_names)
}

#[derive(ClapParser, Debug)]
struct Args {
    input_file: String,

    /// Generated client output
    #[clap(short, long, default_value = "./client.ts")]
    client_output: String,

    /// Generated server output
    #[clap(short, long, default_value = "./server.js")]
    server_output: String,

    /// Generated model output
    #[clap(short, long, default_value = "./model.ts")]
    model_output: String,

    /// Generated certification output
    #[clap(short = 't', long, default_value = "./state-transition-properties.ts")]
    state_transition_properties_output: String,
}

fn main() {
    let args = Args::parse();
    let source = std::fs::read_to_string(args.input_file).expect("No input file provided.");
    let result = LangParser::parse(Rule::Program, &source);
    let mut schemas: Schemas = HashMap::new();
    let mut statements: Vec<AstNode> = vec![];
    let mut js_model: Vec<String> = vec![];
    let mut js_expanded_client: Vec<String> = vec![];
    let mut js_expanded_server: Vec<String> = vec![];
    let mut certification_property_strs: Vec<String> = vec![];
    let mut certification_property_names: Vec<String> = vec![];

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
                // js_translate is a direct translation, i.e. can contain conventions allowed in
                // Sligh that aren't allowed in JS
                let js_ast = js_translate(parsed.clone());

                // js_executable_translate converts Sligh constructs to executable JS, e.g. by
                // replacing state transitions with array operations.
                // TODO: This also has to prepare model for testing, i.e. add an id param to create functions
                js_model.push(js_gen_string(js_executable_translate(&js_ast)));

                let partitioned_class_defs = js_partition_class_definitions(&js_ast);
                let (client, server) = js_infra_expand(js_ast, &schemas, &partitioned_class_defs);
                js_expanded_client.push(js_gen_string(client.clone()));

                for endpoint in server {
                    let endpoint_str = js_gen_string(endpoint);
                    js_expanded_server.push(endpoint_str.clone());
                }

                let (certification_properties, mut names) =
                    js_gen_certification_properties(&partitioned_class_defs, &schemas);
                    
                for property in certification_properties {
                    certification_property_strs.push(js_gen_string(property));
                }
                certification_property_names.append(&mut names);
            }
        }
        Err(e) => println!("Error {:?}", e),
    }

    let client_code = js_expanded_client.join("\n\n");
    fs::write(args.client_output, client_code).expect("Unable to write client code file.");

    let server_code = js_expanded_server.join("\n\n");
    fs::write(args.server_output, server_code).expect("Unable to write server code file.");

    let model_code = js_model.join("\n\n");
    fs::write(args.model_output, model_code).expect("Unable to write model code file.");

    let mut certification_file: Vec<String> = vec![
        "import 'mocha'; \
        import { expect } from \"chai\"; \
        import fc from \"fast-check\"; \
        import { Budget as Fullstack,  } from \"./generated-client\"; \
        import { Budget as Model } from \"./generated-model\"".to_string()
    ];
    certification_file.push(certification_property_strs.join("\n\n"));

    /*
    export const transitionProperties = [
        { name: "createRecurringTransaction", property: createRecurringTransactionsProperty() }
    ];
    */
    let mut property_objects: Vec<String> = vec![];
    for name in certification_property_names {
        let property_obj = JSAstNode::Object { props: vec![
            Prop { key: JSAstNode::Identifier("name".to_string()), value: JSAstNode::StringLiteral(format!("{}", name)) },
            Prop { key: JSAstNode::Identifier("property".to_string()), value: JSAstNode::FuncCallExpr{ call_name: Box::new(JSAstNode::Identifier(format!("{}Property", name))), args: vec![] } }
        ]};
        property_objects.push(js_gen_string(property_obj))
    }
    
    let mut transition_properties: Vec<String> = vec!["export const transitionProperties = [".to_string()];
    let property_objects_str = property_objects.join(",\n");
    let end_array = "];".to_string();
    transition_properties.push(property_objects_str);
    transition_properties.push(end_array);

    certification_file.push(transition_properties.join("\n"));

    fs::write("./certification-properties.ts", certification_file.join("\n\n"))
        .expect("Unable to write certification properties file.");
}

// Goal:
//
// budget.recurring_transactions
//       .map { |rt| rt.expand(start_date, end_date) }
//       .flatten
//       .read!()

// Client:
//
// fetch("/scheduled_transactions")
//
// Server:
//
// res.send(
//   (SELECT * from recurring_transactions)
//     .map { |rt| rt.expand(start_date, end_date) }
//     .flatten)
// )
//

// TODO:
// Lang -
// top-level functions
// let expressions
// statement / expr list
// closures
// trailing closures
// if statement
//
// Infrastructure expansion -
// Endpoint for derived state (recurring_transactions)
