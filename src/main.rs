use std::collections::HashSet;
use clap::Parser as ClapParser;
use convert_case::{Case, Casing};
use pest::iterators::Pair;
use pest::{self, Parser as PestParser};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fs;

const DEBUG: bool = false;
const API_HOST: &str = "http://localhost:3000";

// JS Translation

#[derive(Debug, Clone)]
struct Prop {
    key: JSAstNode,
    value: JSAstNode,
}

// This is more of an intermediate representation. Not necessarily
// executable JS.
#[derive(Debug, Clone)]
enum JSAstNode {
    InvalidNode,
    InterfaceDef {
        name: Box<JSAstNode>,
        properties: Vec<JSAstNode>,
    },
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
        return_type: Option<Box<JSAstNode>>,
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
    ArrayType(Box<JSAstNode>),
    TypedIdentifier {
        identifier: Box<JSAstNode>, // Identifier,
        r#type: Box<JSAstNode>,     // Identifier | ArrayType
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
        JSAstNode::InterfaceDef { name, properties } => {
            let semicolon_separated_properties: String = properties
                .into_iter()
                .map(|prop| js_gen_string(prop))
                .collect::<Vec<String>>()
                .join(";");

            format!(
                "interface {} {{ {} }}",
                js_gen_string(*name),
                semicolon_separated_properties
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
            match *r#type {
                JSAstNode::Identifier(i) => format!("{}: {}", iden_str, i),
                JSAstNode::ArrayType(i) => format!("{}: {}[]", iden_str, js_gen_string(*i)),
                _ => panic!("Should only be translating types"),
            }
        }
        JSAstNode::ArrayType(iden) => js_gen_iden_name(*iden),
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
        /*JSAstNode::InvalidNode => "invalid_node".to_string(),*/
        _ => "".to_string(),
    }
}

fn js_gen_iden_name(node: JSAstNode) -> String {
    match node {
        JSAstNode::Identifier(name) => name,
        _ => panic!("Invalid JS identifier"),
    }
}

fn js_translate_expr(expr: &AstExpr) -> JSAstNode {
    match expr {
        AstExpr::CallExpr {
            receiver,
            call_name,
            args,
        } => {
            if call_name.name == "new" {
                JSAstNode::NewClass {
                    name: Box::new(JSAstNode::Identifier(receiver.name.clone())),
                    args: args.iter().map(js_translate_expr).collect(),
                }
            } else {
                JSAstNode::CallExpr {
                    receiver: Box::new(JSAstNode::Identifier(receiver.name.clone())),
                    call_name: Box::new(JSAstNode::Identifier(call_name.name.clone())),
                    args: args.into_iter().map(js_translate_expr).collect(),
                }
            }
        }
        AstExpr::FuncCall { call_name, args } => JSAstNode::FuncCallExpr {
            call_name: Box::new(JSAstNode::Identifier(call_name.name.clone())),
            args: args.into_iter().map(js_translate_expr).collect(),
        },
        AstExpr::DotAccess { receiver, property } => {
            JSAstNode::Identifier(format!("{}.{}", receiver.name, property.name))
        }
        AstExpr::NumberLiteral(i) => JSAstNode::Identifier(i.to_string()),
        AstExpr::Identifier(i) => JSAstNode::Identifier(i.name.clone()),
        _ => JSAstNode::InvalidNode,
    }
}

fn js_translate_statement(stmt: &AstStatement) -> JSAstNode {
    match stmt {
        AstStatement::LetDecl { name, value } => JSAstNode::LetExpr {
            name: Box::new(JSAstNode::Identifier(name.name.clone())),
            value: Box::new(js_translate_expr(&value)),
        },
        AstStatement::Expr(e) => js_translate_expr(&e),
    }
}

fn js_translate_schema_method(
    name: &AstIdentifier,
    args: &Vec<TypedIdentifier>,
    body: &AstStatementList,
) -> JSAstNode {
    let js_name = JSAstNode::Identifier(name.name.clone());
    let mut js_args: Vec<JSAstNode> = vec![];
    for arg in args {
        js_args.push(JSAstNode::TypedIdentifier {
            identifier: Box::new(JSAstNode::Identifier(arg.identifier.name.clone())),
            r#type: Box::new(js_translate_type(&arg.r#type)),
        });
    }

    JSAstNode::ClassMethod {
        name: Box::new(js_name),
        args: js_args,
        body: Box::new(js_translate_statement_list(&body)),
    }
}

fn js_translate_statement_list(stmt_list: &AstStatementList) -> JSAstNode {
    let mut js_statements: Vec<JSAstNode> = vec![];
    for stmt in &stmt_list.statements {
        js_statements.push(js_translate_statement(stmt));
    }

    JSAstNode::StatementList {
        statements: js_statements,
    }
}

fn js_translate_type(r#type: &Type) -> JSAstNode {
    match r#type {
        Type::Primitive(pt) => match pt {
            PrimitiveType::Int | PrimitiveType::Numeric => {
                JSAstNode::Identifier("number".to_string())
            }
            PrimitiveType::String => JSAstNode::Identifier("string".to_string()),
            PrimitiveType::Array(t) => JSAstNode::ArrayType(Box::new(js_translate_type(&*t))),
        },
        Type::Custom(ct) => match ct {
            CustomType::Schema(s) => JSAstNode::Identifier(s.clone()),
            CustomType::Variant => panic!("Unimplemented JS translation for Variant"),
            CustomType::Function { .. } => panic!("Unimplemented JS translation for Function type"),
        },
        _ => panic!("Unimplemented JS translation for Generic type"),
    }
}

fn js_translate_typed_identifier(typed_iden: &TypedIdentifier) -> JSAstNode {
    JSAstNode::TypedIdentifier {
        identifier: Box::new(JSAstNode::Identifier(typed_iden.identifier.name.clone())),
        r#type: Box::new(js_translate_type(&typed_iden.r#type)),
    }
}

fn js_translate_schema_attribute(name: &AstIdentifier, r#type: &Type) -> JSAstNode {
    JSAstNode::ClassProperty {
        typed_identifier: Box::new(JSAstNode::TypedIdentifier {
            identifier: Box::new(JSAstNode::Identifier(name.name.clone())),
            r#type: Box::new(js_translate_type(r#type)),
        }),
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
        AstNode::SchemaDef { name, body } => {
            let no_methods = body.clone().into_iter().all(|def| match def {
                SchemaDefinition::SchemaAttribute { .. } => true,
                _ => false,
            });

            if no_methods {
                JSAstNode::InterfaceDef {
                    name: Box::new(JSAstNode::Identifier(name.name)),
                    properties: body
                        .into_iter()
                        .map(|def| match def {
                            SchemaDefinition::SchemaAttribute { name, r#type } => {
                                JSAstNode::TypedIdentifier {
                                    identifier: Box::new(JSAstNode::Identifier(name.name)),
                                    r#type: Box::new(js_translate_type(&r#type)),
                                }
                            }
                            _ => panic!("Expected AstNode::SchemaAttribute"),
                        })
                        .collect(),
                }
            } else {
                let mut js_definitions: Vec<JSAstNode> = vec![];
                for def in body {
                    match def {
                        SchemaDefinition::SchemaMethod(SchemaMethod {
                            name, args, body, ..
                        }) => js_definitions.push(js_translate_schema_method(&name, &args, &body)),
                        SchemaDefinition::SchemaAttribute { name, r#type } => {
                            js_definitions.push(js_translate_schema_attribute(&name, &r#type))
                        }
                    }
                }
                let body = JSAstNode::ClassBody {
                    definitions: js_definitions,
                };
                JSAstNode::ClassDef {
                    name: Box::new(JSAstNode::Identifier(name.name)),
                    body: Box::new(body),
                }
            }
        }
        AstNode::FunctionDef(fd) => {
            let name = &fd.name;
            let args = &fd.args;
            let body = &fd.body;
            let mut js_args: Vec<JSAstNode> = vec![];
            for arg in args {
                js_args.push(js_translate_typed_identifier(&arg));
            }

            JSAstNode::FuncDef {
                name: Box::new(JSAstNode::Identifier(name.name.clone())),
                args: js_args,
                body: Box::new(js_translate_statement_list(&body)),
                return_type: None,
            }
        }
        _ => JSAstNode::InvalidNode,
    }
}

// Infrastructure expansion

fn is_state_transition(func_name: &str) -> bool {
    let name_chars: Vec<char> = func_name.chars().collect();

    *name_chars.last().unwrap() == '!'
}    

fn js_state_var_endpoint_server(state_var: &str, st_func: &StateTransferFunc) -> String {
    match st_func {
        StateTransferFunc::Create | StateTransferFunc::Read => format!("/{}", state_var),
        StateTransferFunc::Delete | StateTransferFunc::Update => format!("/{}/:id", state_var),
    }
}

// TODO: Rename to StateTransferFunc
#[derive(Debug, Clone)]
enum StateTransferFunc {
    Create,

    // Note: Read is considered a state transition because it queries the
    // server state and applies it to the client state. It isn't a _read_ of the state,
    // aka state function, but a transition where the client state is updated
    Read,
    Update,
    Delete,
}

impl StateTransferFunc {
    fn as_http_method(&self) -> &'static str {
        match self {
            StateTransferFunc::Create => "POST",
            StateTransferFunc::Read => "GET",
            StateTransferFunc::Update => "PUT",
            StateTransferFunc::Delete => "DELETE",
        }
    }

    fn as_express_http_method(&self) -> &'static str {
        match self {
            StateTransferFunc::Create => "post",
            StateTransferFunc::Read => "get",
            StateTransferFunc::Update => "put",
            StateTransferFunc::Delete => "delete",
        }
    }
}

fn state_transition_func_from_str(s: &str) -> StateTransferFunc {
    match s {
        "create!" => StateTransferFunc::Create,
        "read!" => StateTransferFunc::Read,
        "update!" => StateTransferFunc::Update,
        "delete!" => StateTransferFunc::Delete,
        _ => panic!("Unexpected StateTransferFunc string"),
    }
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

fn js_state_query_read(state_var: &str, server_stmts: &Vec<SLIRServerNode>) -> JSAstNode {
    let mut pre_query_stmts: Vec<JSAstNode> = vec![];
    let mut query_stmts: Vec<&StateQuery> = vec![];
    let mut post_query_js: Vec<JSAstNode> = vec![];

    let mut query_occurred = false;
    for stmt in server_stmts {
        match stmt {
            SLIRServerNode::Logic(stmt) => {
                if !query_occurred {
                    pre_query_stmts.push(js_translate_statement(stmt));
                } else {
                    post_query_js.push(js_translate_statement(stmt));
                }
            }
            SLIRServerNode::StateQuery(sq) => {
                query_occurred = true;
                query_stmts.push(sq);
            }
        }
    }

    // TODO: Only handling one query per action now.
    let query = query_stmts[0];
    let query_result_var = match &query.result_var {
        Some(r) => r.name.to_string(),
        None => "rows".to_string(),
    };

    let sql = SQLAstNode::Select {
        from: Some(Box::new(SQLAstNode::Identifier(
            query.collection.identifier.name.clone(),
        ))),
        attributes: vec![SQLAstNode::Identifier("*".to_string())],
        clause: None,
    };
    let body = if server_stmts.len() == 0 {
        Box::new(JSAstNode::StatementList {
            statements: vec![JSAstNode::CallExpr {
                receiver: Box::new(JSAstNode::Identifier("res".to_string())),
                call_name: Box::new(JSAstNode::Identifier("send".to_string())),
                args: vec![JSAstNode::Identifier("rows".to_string())],
            }],
        })
    } else {
        let last_statement_var_name = match post_query_js.last() {
            Some(stmt) => match stmt {
                JSAstNode::LetExpr { name, .. } => js_gen_iden_name(*name.clone()),
                _ => "rows".to_string(),
            },
            None => "rows".to_string(),
        };

        post_query_js.push(JSAstNode::CallExpr {
            receiver: Box::new(JSAstNode::Identifier("res".to_string())),
            call_name: Box::new(JSAstNode::Identifier("send".to_string())),
            args: vec![JSAstNode::Identifier(last_statement_var_name.to_string())],
        });

        Box::new(JSAstNode::StatementList {
            statements: post_query_js,
        })
    };

    JSAstNode::CallExpr {
        receiver: Box::new(JSAstNode::Identifier("db".to_string())),
        call_name: Box::new(JSAstNode::Identifier("all".to_string())),
        args: vec![
            JSAstNode::StringLiteral(sql_gen_string(&sql)),
            JSAstNode::ArrowClosure {
                args: vec![
                    JSAstNode::Identifier("_".to_string()),
                    JSAstNode::Identifier(query_result_var),
                ],
                body: body,
            },
        ],
    }
}

fn js_state_query_create(
    state_var: &str,
    state_var_type: &str,
    transfer_args: &Vec<TypedAstExpr>,
    schemas: &Schemas,
) -> JSAstNode {
    if transfer_args.len() == 0 {
        return JSAstNode::InvalidNode;
    }

    let mut attr_names: Vec<String> = vec![];
    let mut sql_attr_names: Vec<SQLAstNode> = vec![];
    let mut sql_value_placeholders: Vec<SQLAstNode> = vec![];
    let mut js_attr_values: Vec<JSAstNode> = vec![];
    let mut response_props: Vec<Prop> = vec![];

    let create_arg = &transfer_args[0];
    let arg_type = match &create_arg.r#type {
        Type::Custom(ct) => match ct {
            CustomType::Schema(s) => s.clone(),
            _ => panic!("Only custom types supported"),
        },
        _ => panic!("Only custom types supported"),
    };
    let schema = &schemas[&arg_type];
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
            if !is_state_transition(&call_str) {
                return node.clone();
            }

            let state_trans_func = state_transition_func_from_str(&call_str);
            let state_var = js_gen_string(*receiver.clone());
            match state_trans_func {
                StateTransferFunc::Create => {
                    let state_var_val = js_gen_string(args[0].clone());
                    js_push_var(&state_var, &state_var_val)
                }
                StateTransferFunc::Delete => {
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

fn schema_name_from_type(r#type: &Type) -> String {
    println!("Getting schema name from type: {:#?}", r#type);
    match r#type {
        Type::Custom(ct) => match ct {
            CustomType::Schema(schema_name) => schema_name.to_string(),
            _ => panic!("Can only get schema name from Schema type"),
        },
        _ => panic!("Can only get schema name from Schema type"),
    }
}

fn relation_name_from_type(r#type: &Type) -> String {
    match r#type {
        Type::Primitive(pt) => match pt {
            PrimitiveType::Array(t) => match &**t {
                Type::Custom(custom_type) => match custom_type {
                    CustomType::Schema(schema_name) => {
                        let mut relation_name = schema_name.to_case(Case::Snake);
                        relation_name.push('s');

                        relation_name
                    }
                    _ => panic!("Can only get relation name from Array types"),
                },
                _ => panic!("Can only get relation name from Array types"),
            },
            _ => panic!("No"),
        },
        _ => panic!("Can only get relation name from Array types"),
    }
}

#[derive(pest_derive::Parser)]
#[grammar = "grammar.pest"]
struct LangParser;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct AstIdentifier {
    name: String,
}

#[derive(Debug, Clone)]
struct MethodArgs {
    name: Box<AstNode>,
    arguments: Vec<AstNode>,
}

#[derive(Debug, Clone)]
enum AstExpr {
    CallExpr {
        receiver: AstIdentifier,
        call_name: AstIdentifier,
        args: Vec<AstExpr>,
    },
    FuncCall {
        call_name: AstIdentifier,
        args: Vec<AstExpr>,
    },
    DotAccess {
        receiver: AstIdentifier,
        property: AstIdentifier,
    },
    NumberLiteral(i64),
    Identifier(AstIdentifier),
}

#[derive(Debug, Clone)]
struct TypedAstExpr {
    r#type: Type,
    expr: AstExpr,
}

#[derive(Debug, Clone)]
enum AstStatement {
    LetDecl { name: AstIdentifier, value: AstExpr },
    Expr(AstExpr),
}

#[derive(Debug, Clone)]
struct AstStatementList {
    statements: Vec<AstStatement>,
}

#[derive(Debug, Clone)]
struct SchemaMethod {
    name: AstIdentifier,
    args: Vec<TypedIdentifier>,
    body: AstStatementList,
    return_type: Option<Type>,
}

// Rename SchemaAttribute struct to TypedIdentifier
#[derive(Debug, Clone)]
enum SchemaDefinition {
    SchemaAttribute { name: AstIdentifier, r#type: Type },
    SchemaMethod(SchemaMethod),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct TypedIdentifier {
    identifier: AstIdentifier,
    r#type: Type,
}

#[derive(Debug, Clone)]
struct ArrayType {
    r#type: AstIdentifier,
}

#[derive(Debug, Clone)]
struct AstFunctionDef {
    name: AstIdentifier,
    body: AstStatementList,
    args: Vec<TypedIdentifier>,
    type_params: Option<Vec<Type>>,
    return_type: Option<Type>,
}

#[derive(Debug, Clone)]
enum AstNode {
    InvalidNode,
    SchemaDef {
        name: AstIdentifier,
        body: Vec<SchemaDefinition>,
    },
    FunctionDef(AstFunctionDef),
}

fn parse_type(pair: pest::iterators::Pair<Rule>, schemas: &Schemas) -> Type {
    match pair.as_rule() {
        Rule::TypedIdentifier => parse_type(pair.into_inner().next().unwrap(), schemas),
        Rule::Type => {
            let iden = pair.into_inner().next().unwrap().as_str();

            type_from_str(iden, schemas)
        }
        Rule::ArrayType => {
            let type_iden = pair.into_inner().next().unwrap().as_str();
            let r#type = type_from_str(&type_iden, schemas);

            Type::Primitive(PrimitiveType::Array(Box::new(r#type)))
        }
        other => panic!("Attempted to parse unexpected type: {:?}", other),
    }
}

fn parse_typed_identifier(pair: pest::iterators::Pair<Rule>, schemas: &Schemas) -> TypedIdentifier {
    match pair.as_rule() {
        Rule::TypedIdentifier => {
            let mut typed_iden = pair.into_inner();
            let iden = typed_iden.next().unwrap().as_str();
            let r#type = parse_type(typed_iden.next().unwrap(), schemas);

            TypedIdentifier {
                identifier: AstIdentifier {
                    name: iden.to_string(),
                },
                r#type: r#type,
            }
        }
        other => panic!(
            "Found unknown node during type identifier parsing: {:?}",
            other
        ),
    }
}

fn parse_expr(pair: pest::iterators::Pair<Rule>) -> AstExpr {
    match pair.as_rule() {
        Rule::NumberLiteral => AstExpr::NumberLiteral(pair.as_str().parse::<i64>().unwrap()),
        Rule::Identifier => AstExpr::Identifier(AstIdentifier {
            name: pair.as_str().to_string(),
        }),
        Rule::MethodCall => {
            let mut expr = pair.into_inner();
            let receiver = expr.next().unwrap().as_str();
            let call_name = expr.next().unwrap().as_str();
            let mut args: Vec<AstExpr> = vec![];
            for call_arg in expr {
                args.push(parse_expr(call_arg))
            }

            AstExpr::CallExpr {
                receiver: AstIdentifier {
                    name: receiver.to_string(),
                },
                call_name: AstIdentifier {
                    name: call_name.to_string(),
                },
                args: args,
            }
        }
        Rule::FuncCall => {
            let mut func_call = pair.into_inner();
            let call_name = func_call.next().unwrap().as_str();
            let mut args: Vec<AstExpr> = vec![];
            for call_arg in func_call {
                args.push(parse_expr(call_arg))
            }

            AstExpr::FuncCall {
                call_name: AstIdentifier {
                    name: call_name.to_string(),
                },
                args: args,
            }
        }
        Rule::MethodArg => parse_expr(pair.into_inner().next().unwrap()),
        Rule::DotAccess => {
            let mut dot_access = pair.into_inner();
            let receiver = dot_access.next().unwrap().as_str();
            let property = dot_access.next().unwrap().as_str();

            AstExpr::DotAccess {
                receiver: AstIdentifier {
                    name: receiver.to_string(),
                },
                property: AstIdentifier {
                    name: property.to_string(),
                },
            }
        }
        other => panic!("Found unknown node during expression parsing: {:?}", other),
    }
}

fn parse_statement(pair: pest::iterators::Pair<Rule>) -> AstStatement {
    match pair.as_rule() {
        Rule::LetExpr => {
            let mut let_expr = pair.into_inner();
            let iden = let_expr.next().unwrap().as_str();
            let value = parse_expr(let_expr.next().unwrap());

            AstStatement::LetDecl {
                name: AstIdentifier {
                    name: iden.to_string(),
                },
                value: value,
            }
        }
        Rule::MethodCall => AstStatement::Expr(parse_expr(pair)),
        Rule::FuncCall => AstStatement::Expr(parse_expr(pair)),
        other => panic!("Unexpected node during statement parsing: {:?}", other),
    }
}

fn parse_function_like(
    pair: pest::iterators::Pair<Rule>,
    schemas: &Schemas,
) -> (
    AstIdentifier,
    Vec<TypedIdentifier>,
    AstStatementList,
    Option<Type>,
) {
    let mut schema_method = pair.into_inner();

    let mut method_args = schema_method.next().unwrap().into_inner();
    let method_name = method_args.next().unwrap().as_str();
    let mut args: Vec<TypedIdentifier> = vec![];
    for method_arg in method_args {
        args.push(parse_typed_identifier(method_arg, schemas))
    }

    let return_type_or_body = schema_method.next().unwrap();
    let mut return_type: Option<Type> = None;
    let mut body = return_type_or_body.clone();
    match return_type_or_body.as_rule() {
        Rule::Identifier => {
            return_type = Some(type_from_str(return_type_or_body.as_str(), schemas));
            body = schema_method.next().unwrap()
        }
        Rule::MethodBody => (),
        _ => panic!("Uknown node when parsing method definition"),
    }

    let method_statement_list = body.into_inner().next().unwrap().into_inner();

    let mut statements: Vec<AstStatement> = vec![];
    for method_statement in method_statement_list {
        statements.push(parse_statement(method_statement));
    }

    (
        AstIdentifier {
            name: method_name.to_string(),
        },
        args,
        AstStatementList {
            statements: statements,
        },
        return_type,
    )
}

fn parse_schema_def(pair: pest::iterators::Pair<Rule>, schemas: &Schemas) -> SchemaDefinition {
    match pair.as_rule() {
        Rule::SchemaAttribute => {
            let mut typed_iden = pair.into_inner().next().unwrap().into_inner();
            let attr_name = typed_iden.next().unwrap().as_str();
            let attr_type = parse_type(typed_iden.next().unwrap(), schemas);

            SchemaDefinition::SchemaAttribute {
                name: AstIdentifier {
                    name: attr_name.to_string(),
                },
                r#type: attr_type,
            }
        }
        Rule::SchemaMethod => {
            let (name, args, body, return_type) = parse_function_like(pair, schemas);
            return SchemaDefinition::SchemaMethod(SchemaMethod {
                name: name,
                args: args,
                body: body,
                return_type: return_type,
            });
        }
        other => panic!(
            "Unexpected node during SchemaDefinition parsing: {:?}",
            other
        ),
    }
}

fn parse(pair: pest::iterators::Pair<Rule>, schemas: &Schemas) -> AstNode {
    if DEBUG {
        println!("Parsing");
        println!("{}", pair.to_json());
    }
    match pair.as_rule() {
        Rule::Statement => parse(pair.into_inner().next().unwrap(), schemas),
        Rule::SchemaDef => {
            let mut inner = pair.into_inner();
            let name = inner.next().unwrap().as_str();
            let schema_body = inner.next().unwrap();
            let mut schema_defs: Vec<SchemaDefinition> = vec![];
            for def in schema_body.into_inner() {
                schema_defs.push(parse_schema_def(def, schemas));
            }

            AstNode::SchemaDef {
                name: AstIdentifier {
                    name: name.to_string(),
                },
                body: schema_defs,
            }
        }
        Rule::SchemaMethod => {
            let (name, args, body, return_type) = parse_function_like(pair, schemas);

            AstNode::FunctionDef(AstFunctionDef {
                name: name,
                args: args,
                body: body,
                type_params: None,
                return_type: return_type,
            })
        }
        _ => {
            if DEBUG {
                println!("Attempted to parse unknown node");
            }

            AstNode::InvalidNode
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

#[derive(Clone, Debug)]
struct SchemaAttribute {
    name: String,
    r#type: Type,
}

#[derive(Clone, Debug)]
struct Schema {
    name: String,
    attributes: Vec<SchemaAttribute>,
}

fn type_from_str(type_str: &str, schemas: &Schemas) -> Type {
    if let Some(primitive_type) = match type_str {
        "Int" => Some(Type::Primitive(PrimitiveType::Int)),
        "Numeric" => Some(Type::Primitive(PrimitiveType::Numeric)),
        "String" => Some(Type::Primitive(PrimitiveType::String)),
        _ => None,
    } {
        primitive_type
    } else {
        let schema = schemas.get(type_str);
        if !schema.is_some() {
            panic!("Unable to find schema during attribute type resolution")
        }

        Type::Custom(CustomType::Schema(schema.unwrap().name.clone()))
    }
}

// Translation Certification

fn fast_check_arbitrary_from_type(r#type: &Type) -> String {
    match r#type {
        Type::Primitive(pt) => match pt {
            PrimitiveType::String => "string".to_string(),
            PrimitiveType::Numeric => "float".to_string(),
            _ => panic!("Trying to convert unknown primitive type to a fast-check arbitrary"),
        },
        _ => panic!("Trying to convert unknown type name to a fast-check arbitrary"),
    }
}

fn certification_determine_state_transfer(
    body: &AstStatementList,
) -> Option<(StateTransferFunc, String)> {
    for stmt in &body.statements {
        match stmt {
            AstStatement::Expr(e) => match e {
                AstExpr::CallExpr {
                    call_name,
                    receiver,
                    ..
                } => {
                    if is_state_transition(&call_name.name) {
                        return Some((
                            state_transition_func_from_str(&call_name.name),
                            receiver.name.clone(),
                        ));
                    }
                }
                _ => continue,
            },
            _ => continue,
        }
    }

    return None;
}

fn slir_gen_certification_properties(
    state_transfers: &Vec<SchemaMethod>,
    schemas: &Schemas,
) -> (Vec<JSAstNode>, Vec<String>) {
    let mut js_property_defs: Vec<JSAstNode> = vec![];
    let mut js_property_names: Vec<String> = vec![];
    let state_transitions = state_transfers;
    for state_transition in state_transitions {
        println!(
            "Certification properties for method: {}",
            state_transition.name.name
        );
        let state_trans_name = &state_transition.name;
        let (state_trans_func, state_variable) =
            certification_determine_state_transfer(&state_transition.body)
                .expect("Currently expecting a single state transfer func in all actions");
        js_property_names.push(state_trans_name.name.clone());

        match state_trans_func {
            StateTransferFunc::Create => {
                // Create data generators for all transition arguments
                let mut data_generators: Vec<JSAstNode> = vec![];
                // Argument list for property body
                let mut generated_data_vars: Vec<JSAstNode> = vec![];
                for arg in &state_transition.args {
                    let schema_name = schema_name_from_type(&arg.r#type);
                    let schema = &schemas[&schema_name];
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
                    generated_data_vars.push(JSAstNode::Identifier(arg.identifier.name.clone()));
                }

                let property_func_name = format!("{}Property", state_trans_name.name);

                // TODO: For each state variable, add a data generator for it here
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
                                        state_trans_name.name.to_string(),
                                    )),
                                    // args[0] again - represents a state transition argument
                                    args: vec![JSAstNode::Identifier(
                                        state_transition.args[0].identifier.name.clone(),
                                    )],
                                }),
                            }),
                        },
                        JSAstNode::CallExpr {
                            receiver: Box::new(JSAstNode::Identifier("model".to_string())),
                            call_name: Box::new(JSAstNode::Identifier(
                                state_trans_name.name.to_string(),
                            )),
                            args: vec![
                                JSAstNode::Identifier(
                                    state_transition.args[0].identifier.name.clone(),
                                ),
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
                                    state_variable
                                ))],
                            }),
                            call_name: Box::new(JSAstNode::Identifier("to.deep.eq".to_string())),
                            args: vec![JSAstNode::Identifier(format!("model.{}", state_variable))],
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
                    return_type: None,
                };

                js_property_defs.push(property_func)
            }
            _ => (),
        }
    }

    (js_property_defs, js_property_names)
}

fn gen_server_endpoint_file(endpoints: &Vec<JSAstNode>) -> String {
    let define_endpoints_func = JSAstNode::FuncDef {
        name: Box::new(JSAstNode::Identifier("defineEndpoints".to_string())),
        args: vec![
            JSAstNode::Identifier("app".to_string()),
            JSAstNode::Identifier("db".to_string()),
        ],
        body: Box::new(JSAstNode::StatementList {
            statements: endpoints.clone(),
        }),
        return_type: None,
    };

    return js_gen_string(define_endpoints_func);
}

fn gen_server_file(endpoints: &Vec<JSAstNode>, function_defs: &Vec<AstFunctionDef>) -> String {
    let function_strs: Vec<String> = function_defs
        .iter()
        .map(|def| js_gen_string(js_translate(AstNode::FunctionDef(def.clone()))))
        .collect();
    let endpoint_str = gen_server_endpoint_file(endpoints);

    let functions = function_strs.join("\n\n");

    vec![functions, endpoint_str].join("\n\n")
}

fn gen_certification_property_file(
    certification_properties: &Vec<String>,
    certification_property_names: &Vec<String>,
) -> Vec<String> {
    let mut certification_file: Vec<String> = vec!["import 'mocha';\n\
        import { expect } from \"chai\";\n\
        import fc from \"fast-check\"\n\
        import { Budget as Fullstack,  } from \"./generated-client\";\n\
        import { Budget as Model } from \"./generated-model\"\n"
        .to_string()];
    certification_file.push(certification_properties.join("\n\n"));

    let mut property_objects: Vec<String> = vec![];
    for name in certification_property_names {
        let property_obj = JSAstNode::Object {
            props: vec![
                Prop {
                    key: JSAstNode::Identifier("name".to_string()),
                    value: JSAstNode::StringLiteral(format!("{}", name)),
                },
                Prop {
                    key: JSAstNode::Identifier("property".to_string()),
                    value: JSAstNode::FuncCallExpr {
                        call_name: Box::new(JSAstNode::Identifier(format!("{}Property", name))),
                        args: vec![],
                    },
                },
            ],
        };
        property_objects.push(js_gen_string(property_obj))
    }

    let mut transition_properties: Vec<String> =
        vec!["export const transitionProperties = [".to_string()];
    let property_objects_str = property_objects.join(",\n");
    let end_array = "];".to_string();
    transition_properties.push(property_objects_str);
    transition_properties.push(end_array);

    certification_file.push(transition_properties.join("\n"));

    certification_file
}

// Type system

// To enforce uniqueness of names in the type environment
fn type_env_name(name_path: &Vec<String>) -> String {
    name_path.join(".")
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum PrimitiveType {
    // Integers
    Int,
    // Fixed-point real numbers
    Numeric,
    // Strings
    String,
    Array(Box<Type>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum CustomType {
    Schema(String), // Schema name
    Function {
        args: Vec<Type>,
        type_params: Option<Vec<Type>>,
        return_type: Box<Option<Type>>,
    },
    Variant,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Type {
    Primitive(PrimitiveType),
    Custom(CustomType),
    Generic(String),
}

type TypeEnvironment = HashMap<String, Type>;

// Resolves the type of a variable by searching through parent nodes
fn resolve_variable_type(name_path: &Vec<String>, type_env: &TypeEnvironment) -> Option<Type> {
    let qualified_name = type_env_name(name_path);
    match type_env.get(&qualified_name) {
        Some(t) => Some(t).cloned(),
        None => {
            if name_path.len() == 1 {
                None
            } else {
                let mut parent_name_path = name_path.clone();
                parent_name_path.remove(name_path.len() - 2);
                resolve_variable_type(&parent_name_path, type_env)
            }
        }
    }
}

fn resolve_generics_fn(call_type: &Type, receiver_type: &Type, arg_types: &Vec<Type>) -> Type {
    match call_type {
        Type::Custom(ct) => match ct {
            CustomType::Function {
                args,
                type_params,
                return_type,
            } => {
                match type_params {
                    Some(tp) => {
                        let mut type_instantiation: HashMap<String, Type> = HashMap::new();
                        let mut all_types = vec![receiver_type.clone()];
                        let mut arg_types_m = arg_types.clone();
                        all_types.append(&mut arg_types_m);

                        // println!("Generic function args: {:#?}", args);
                        // println!("Concrete types: {:#?}", all_types);

                        assert!(
                            args.len() == all_types.len(),
                            "Incorrect arity for generic function"
                        );

                        // Basic type inference by matching on similar type structures until a Generic type
                        // is matched with a concrete one
                        for (i, arg_type) in args.clone().into_iter().enumerate() {
                            let concrete_type = &all_types[i];
                            match (arg_type, concrete_type) {
                                (Type::Primitive(pat), Type::Primitive(pct)) => match (pat, pct) {
                                    (PrimitiveType::Array(pt), PrimitiveType::Array(ct)) => {
                                        match (&*pt, &*ct) {
                                            (Type::Generic(pg), ..) => {
                                                type_instantiation
                                                    .insert(pg.to_string(), concrete_type.clone());
                                            }
                                            _ => (),
                                        }
                                    }
                                    _ => (),
                                },
                                (Type::Custom(cat), Type::Custom(cct)) => match (cat, cct) {
                                    (
                                        CustomType::Function {
                                            return_type: arg_return_type,
                                            ..
                                        },
                                        CustomType::Function {
                                            return_type: conc_return_type,
                                            ..
                                        },
                                    ) => match (&*arg_return_type, &**conc_return_type) {
                                        (Some(Type::Generic(ag)), Some(..)) => {
                                            type_instantiation.insert(
                                                ag.to_string(),
                                                conc_return_type.clone().unwrap(),
                                            );
                                        }
                                        _ => (),
                                    },
                                    _ => (),
                                },
                                _ => (),
                            }
                        }

                        match &**return_type {
                            Some(Type::Generic(g)) => {
                                type_instantiation
                                    .get(g)
                                    .expect("Generic type error - could not resolve generic type")
                                    .clone()
                            }
                            Some(Type::Primitive(pt)) => match pt {
                                PrimitiveType::Array(t) => match &**t {
                                    Type::Generic(g) => {
                                        Type::Primitive(PrimitiveType::Array(
                                            Box::new(type_instantiation.get(g).expect("Generic type error - could not resolve generic type").clone())
                                        ))
                                    }
                                    _ => panic!("Could not resolve generic type"),
                                },
                                _ => panic!("Could not resolve generic type"),
                            },
                            _ => panic!("Could not resolve generic type"),
                        }
                    }
                    None => call_type.clone(),
                }
            }
            _ => panic!("Unimplemented generic substitution"),
        },
        _ => call_type.clone(),
    }
}

// Resolves the type of an expression
fn resolve_type(
    node: &AstExpr,
    name_path: &Vec<String>,
    type_env: &TypeEnvironment,
) -> Option<Type> {
    match node {
        AstExpr::DotAccess { receiver, property } => {
            let name_path = vec![receiver.name.clone(), property.name.clone()];
            let qualified_name = type_env_name(&name_path);

            type_env.get(&qualified_name).cloned()
        }
        AstExpr::Identifier(i) => {
            let mut qualified_name_path = name_path.clone();
            qualified_name_path.push(i.name.clone());

            resolve_variable_type(&qualified_name_path, type_env)
        }
        AstExpr::NumberLiteral(..) => Some(Type::Primitive(PrimitiveType::Int)),
        AstExpr::CallExpr {
            receiver,
            call_name,
            args,
        } => {
            let mut call_name_path = name_path.clone();
            call_name_path.push(call_name.name.clone());

            let mut receiver_name_path = name_path.clone();
            receiver_name_path.push(receiver.name.clone());
            let receiver_type = resolve_variable_type(&receiver_name_path, type_env)
                .expect("Type error - function receiver");
            let arg_types = args
                .into_iter()
                .map(|arg| {
                    resolve_type(&arg, name_path, type_env).expect("Type error - function args")
                })
                .collect();

            let call_type = resolve_variable_type(&call_name_path, type_env)
                .expect("Type error - CallExpr call name");
            Some(resolve_generics_fn(&call_type, &receiver_type, &arg_types))
        }
        AstExpr::FuncCall { call_name, args } => {
            // Note: Currently not instantiating generic function types
            let mut call_name_path = name_path.clone();
            call_name_path.push(call_name.name.clone());

            let _: Vec<Type> = args
                .into_iter()
                .map(|arg| {
                    resolve_type(&arg, name_path, type_env).expect("Type error - function args")
                })
                .collect();

            Some(
                resolve_variable_type(&call_name_path, type_env)
                    .expect("Type error - CallExpr call name"),
            )
        }
        _ => None,
    }
}

fn update_type_environment_statement(
    stmt_list: &AstStatementList,
    name_path: Vec<String>,
    type_env: &mut TypeEnvironment,
) {
    for stmt in &stmt_list.statements {
        match stmt {
            AstStatement::LetDecl { name, value } => {
                let err_str = format!("Type error - cannot resolve type for {}", name.name);
                let r#type = resolve_type(&value, &name_path, type_env).expect(&err_str);
                let mut statement_name_path = name_path.clone();
                statement_name_path.push(name.name.clone());

                type_env.insert(type_env_name(&statement_name_path), r#type);
            }
            _ => (),
        }
    }
}

// Add parsed code to various collections of info
fn update_environment(
    pair: &Pair<Rule>,
    node: &AstNode,
    schemas: &mut Schemas,
    function_defs: &mut Vec<AstFunctionDef>,
    type_env: &mut TypeEnvironment,
) {
    // Add to Schemas:
    match node {
        AstNode::SchemaDef { name, body } => {
            let schema_name = name.name.clone();
            let mut attributes: Vec<SchemaAttribute> = vec![];
            for schema_def in body {
                match schema_def {
                    SchemaDefinition::SchemaAttribute { name, r#type } => {
                        attributes.push(SchemaAttribute {
                            name: name.name.clone(),
                            r#type: r#type.clone(),
                        })
                    }
                    _ => continue,
                }
            }
            let schema = Schema {
                name: schema_name.clone(),
                attributes: attributes.clone(),
            };
            schemas.insert(schema_name.clone(), schema);
        }
        _ => (),
    };

    match node {
        AstNode::SchemaDef { name, body } => {
            let schema_name = name.name.clone();
            let schema_name_path = vec![schema_name.clone()];
            type_env.insert(
                type_env_name(&schema_name_path),
                Type::Custom(CustomType::Schema(schema_name.clone())),
            );
            for def in body {
                match def {
                    SchemaDefinition::SchemaAttribute { name, r#type } => {
                        let name_path = vec![schema_name.clone(), name.name.clone()];
                        type_env.insert(type_env_name(&name_path), r#type.clone());
                    }
                    // Todo: This should add function signature to type env, including return type
                    SchemaDefinition::SchemaMethod(SchemaMethod {
                        name, args, body, ..
                    }) => {
                        let method_name = name.name.clone();
                        let method_name_path = vec![schema_name.clone(), method_name];
                        for arg in args {
                            let mut arg_name_path = method_name_path.clone();
                            arg_name_path.push(arg.identifier.name.clone());
                            type_env.insert(type_env_name(&arg_name_path), arg.r#type.clone());
                        }
                        update_type_environment_statement(body, method_name_path.clone(), type_env);
                    }
                }
            }
        }
        AstNode::FunctionDef(fd) => {
            let name = &fd.name;
            let args = &fd.args;
            let type_params = &fd.type_params;
            let return_type = &fd.return_type;

            type_env.insert(
                name.name.clone(),
                Type::Custom(CustomType::Function {
                    args: args.into_iter().map(|arg| arg.r#type.clone()).collect(),
                    type_params: type_params.clone(),
                    return_type: Box::new(return_type.clone()),
                }),
            );

            function_defs.push(fd.clone());
        }
        _ => (), /*println!("Attempted to add type information for unhandled node")*/
    }
}

fn insert_seed_types(type_env: &mut TypeEnvironment) {
    // map: 'a list => ('a => 'b) => 'b list
    type_env.insert(
        "map".to_string(),
        Type::Custom(CustomType::Function {
            args: vec![
                Type::Primitive(PrimitiveType::Array(Box::new(Type::Generic(
                    "a".to_string(),
                )))),
                Type::Custom(CustomType::Function {
                    args: vec![Type::Generic("a".to_string())],
                    type_params: None,
                    return_type: Box::new(Some(Type::Generic("b".to_string()))),
                }),
            ],
            type_params: Some(vec![
                Type::Generic("a".to_string()),
                Type::Generic("b".to_string()),
            ]),
            return_type: Box::new(Some(Type::Primitive(PrimitiveType::Array(Box::new(
                Type::Generic("b".to_string()),
            ))))),
        }),
    );
}

// SLIR

#[derive(Debug, Clone)]
enum Query {
    Where,
}

#[derive(Debug)]
struct StateTransfer {
    name: String,
    collection: TypedIdentifier,
    transition: StateTransferFunc,
    args: Vec<TypedAstExpr>,
    // prob need var name so result of query is what's being transferred
}

#[derive(Debug, Clone)]
struct StateQuery {
    collection: TypedIdentifier,
    query: Query,
    transition: StateTransferFunc,
    result_var: Option<AstIdentifier>,
}

#[derive(Debug)]
enum SLIRNode {
    StateQuery(StateQuery),
    StateTransfer(StateTransfer),
    Logic(AstStatement),
}

#[derive(Debug)]
enum SLIRServerNode {
    StateQuery(StateQuery),
    Logic(AstStatement),
}

fn state_variable_collection_name(
    name: &str,
    name_path: &Vec<String>,
    schemas: &Schemas,
    type_env: &TypeEnvironment,
) -> Option<String> {
    let r#type = resolve_variable_type(&name_path, type_env).expect("Type error - slir_expr_nodes");

    Some(relation_name_from_type(&r#type))
}

fn slir_expr_nodes(
    expr: &AstExpr,
    schema_name: &str,
    method_name: &str,
    stmt: &AstStatement,
    schemas: &Schemas,
    type_env: &TypeEnvironment,
    slir_nodes: &mut Vec<SLIRNode>,
) {
    match expr {
        AstExpr::DotAccess { receiver, property } => {
            let name_path = vec![receiver.name.clone(), property.name.clone()];
            if let Some(collection_name) =
                state_variable_collection_name(&receiver.name, &name_path, schemas, type_env)
            {
                let r#type = resolve_variable_type(&name_path, type_env)
                    .expect("Type error - slir_expr_nodes");
                let result_var = match stmt {
                    AstStatement::LetDecl { name, .. } => Some(AstIdentifier {
                        name: name.name.clone(),
                    }),
                    _ => None,
                };
                slir_nodes.push(SLIRNode::StateQuery(StateQuery {
                    collection: TypedIdentifier {
                        identifier: AstIdentifier {
                            name: collection_name,
                        },
                        r#type: r#type,
                    },
                    query: Query::Where,
                    transition: StateTransferFunc::Read,
                    result_var: result_var,
                }))
            } else {
                slir_nodes.push(SLIRNode::Logic(AstStatement::Expr(expr.clone())));
            }
        }
        AstExpr::CallExpr {
            receiver,
            call_name,
            args,
        } => {
            if is_state_transition(&call_name.name) {
                let name_path = vec![
                    schema_name.to_string(),
                    method_name.to_string(),
                    receiver.name.to_string(),
                ];
                let potential_state_var = name_path.clone().into_iter().last();
                let schema = schemas.get(schema_name).expect("Unable to find schema");
                let is_state_variable = schema
                    .attributes
                    .iter()
                    .any(|attr| attr.name == receiver.name);
                let state_var_type = resolve_variable_type(&name_path, type_env)
                    .expect("Type error - slir translate");
                match state_variable_collection_name(&receiver.name, &name_path, schemas, type_env)
                {
                    Some(collection_name) if is_state_variable => {
                        slir_nodes.push(SLIRNode::StateQuery(StateQuery {
                            collection: TypedIdentifier {
                                identifier: AstIdentifier {
                                    name: collection_name,
                                },
                                r#type: state_var_type.clone(),
                            },
                            query: Query::Where,
                            transition: state_transition_func_from_str(&call_name.name),
                            result_var: None,
                        }));
                    }
                    _ => (),
                };

                let method_path: Vec<String> = name_path
                    .clone()
                    .into_iter()
                    .take(name_path.len() - 1)
                    .collect();
                let typed_args = args
                    .into_iter()
                    .map(|a| {
                        let arg_type = resolve_type(a, &method_path, type_env)
                            .expect("Type error - function arg did no type check");

                        TypedAstExpr {
                            r#type: arg_type,
                            expr: a.clone(),
                        }
                    })
                    .collect();
                slir_nodes.push(SLIRNode::StateTransfer(StateTransfer {
                    name: method_name.to_string(),
                    collection: TypedIdentifier {
                        identifier: AstIdentifier {
                            name: relation_name_from_type(&state_var_type),
                        },
                        r#type: state_var_type,
                    },
                    transition: state_transition_func_from_str(&call_name.name),
                    args: typed_args,
                }))
            } else {
                slir_nodes.push(SLIRNode::Logic(stmt.clone()))
            }
        }
        // This should recursively call slir_expr_nodes on all of its arguments, ex: process(Budget.recurring_transactions)
        AstExpr::FuncCall { args, .. } => {
            for arg in args {
                slir_expr_nodes(
                    arg,
                    schema_name,
                    method_name,
                    stmt,
                    schemas,
                    type_env,
                    slir_nodes,
                );
            }
            slir_nodes.push(SLIRNode::Logic(stmt.clone()))
        }
        _ => {
            println!("Warning - slir_expr_nodes uknown expr type")
        }
    }
}

fn slir_translate(
    schema_name: &str,
    method_name: &str,
    body: &AstStatementList,
    schemas: &Schemas,
    type_env: &TypeEnvironment,
) -> Vec<SLIRNode> {
    let mut slir_nodes: Vec<SLIRNode> = vec![];
    for stmt in &body.statements {
        match stmt {
            AstStatement::LetDecl { name, value } => {
                let mut let_nodes: Vec<SLIRNode> = vec![];
                slir_expr_nodes(
                    &value,
                    schema_name,
                    method_name,
                    stmt,
                    schemas,
                    type_env,
                    &mut let_nodes,
                );
                slir_nodes.append(&mut let_nodes);
            }
            AstStatement::Expr(e) => slir_expr_nodes(
                &e,
                schema_name,
                method_name,
                stmt,
                schemas,
                type_env,
                &mut slir_nodes,
            ),
        }
    }

    slir_nodes
}

// Tier splitting: SLIR -> JS

fn slir_expand_state_transfer_server(
    state_transfer: &StateTransfer,
    server_stmts: &Vec<SLIRServerNode>,
    schemas: &Schemas,
) -> JSAstNode {
    let state_transition_func = &state_transfer.transition;
    let state_var = &state_transfer.collection.identifier.name;
    let express_method = state_transfer.transition.as_express_http_method();

    let endpoint_path = js_state_var_endpoint_server(&state_var, &state_transition_func);
    let state_body = match state_transition_func {
        StateTransferFunc::Create => {
            let state_var_type = &state_transfer.collection.r#type;
            let state_var_str = js_gen_string(js_translate_type(&state_var_type));
            let query =
                js_state_query_create(&state_var, &state_var_str, &state_transfer.args, schemas);
            let parse_data = JSAstNode::LetExpr {
                name: Box::new(JSAstNode::Identifier("data".to_string())),
                value: Box::new(JSAstNode::Identifier("req.body".to_string())),
            };
            Box::new(JSAstNode::StatementList {
                statements: vec![parse_data, query],
            })
        }
        StateTransferFunc::Read => {
            let query = js_state_query_read(&state_var, &server_stmts);
            Box::new(query)
        }
        StateTransferFunc::Delete => {
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

fn slir_expand_fetch_args_from_state_transition(
    st: &StateTransferFunc,
    args: &Vec<AstExpr>,
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
        StateTransferFunc::Create => {
            let js_create_arg = js_translate_expr(&args[0]);
            let state_var_iden = JSAstNode::Identifier(js_gen_string(js_create_arg));
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

fn slir_state_var_endpoint_client(
    state_var: &String,
    state_transition: &StateTransferFunc,
    args: &Vec<AstExpr>,
) -> JSAstNode {
    match state_transition {
        StateTransferFunc::Create | StateTransferFunc::Read => {
            JSAstNode::StringLiteral(format!("{}/{}", API_HOST, state_var))
        }
        StateTransferFunc::Delete | StateTransferFunc::Update => {
            // args[0] really sholuld be a type check. delete! only takes one argument.
            let state_var_arg = js_translate_expr(&args[0]);
            JSAstNode::PlusExpr {
                left: Box::new(JSAstNode::StringLiteral(format!(
                    "{}/{}/",
                    API_HOST, state_var
                ))),
                right: Box::new(JSAstNode::Identifier(format!(
                    "{}.id",
                    js_gen_string(state_var_arg)
                ))),
            }
        }
    }
}

// This turns a semantic state transition into a network request to update the state in
// the database as well as optimistically update the client state
fn slir_expand_state_transfer_client(
    state_trans_func: &StateTransferFunc,
    state_var: &String,
    args: &Vec<AstExpr>,
) -> JSAstNode {
    let endpoint = slir_state_var_endpoint_client(&state_var, &state_trans_func, &args);
    let st_fetch_args = slir_expand_fetch_args_from_state_transition(&state_trans_func, &args);

    let fetch_args = vec![endpoint, st_fetch_args];

    let fetch = JSAstNode::FuncCallExpr {
        call_name: Box::new(JSAstNode::Identifier("fetch".to_string())),
        args: fetch_args,
    };
    let expanded_statements: Vec<JSAstNode> = match state_trans_func {
        StateTransferFunc::Create => {
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
        StateTransferFunc::Delete => {
            let js_arg = js_translate_expr(&args[0]);
            let update_client_state = js_delete_var(&state_var, &js_gen_string(js_arg));
            vec![fetch, update_client_state]
        }
        StateTransferFunc::Read => {
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

fn slir_make_state_transfers_client(
    schema_name: &str,
    state_transfers: &Vec<&StateTransfer>,
    schemas: &Schemas,
    type_env: &TypeEnvironment,
) -> Vec<JSAstNode> {
    let mut class_methods: Vec<JSAstNode> = vec![];
    for transfer in state_transfers {    
        let expanded_client_body = slir_expand_state_transfer_client(
            &transfer.transition,
            &transfer.collection.identifier.name,
            &transfer.args.clone().into_iter().map(|a| a.expr).collect(),
        );
        let class_method = JSAstNode::ClassMethod {
            name: Box::new(JSAstNode::Identifier(transfer.name.clone())),
            args: vec![],
            body: Box::new(expanded_client_body),
        };
        class_methods.push(JSAstNode::AsyncModifier(Box::new(class_method)));
    }

    class_methods
}

fn slir_tier_split(
    schema_name: &str,
    parsed_node: &AstNode,
    slir: &Vec<SLIROperations>,
    schemas: &Schemas,
    type_env: &TypeEnvironment,
) -> (JSAstNode, Vec<JSAstNode>) {
    match parsed_node {
        AstNode::SchemaDef { body, .. } => {
            let no_methods = body.clone().into_iter().all(|def| match def {
                SchemaDefinition::SchemaAttribute { .. } => true,
                _ => false,
            });

            if no_methods {
                return (js_translate(parsed_node.clone()), vec![]);
            }
        },
        _ => ()
    }

    // Simple algorithm to start: Assuming StateTransfers occur at the end of a statement list,
    // the statements before it can be placed on the server, and anything after can be placed
    // on the client. The StateTransfer itself gets compiled into both client and server components.
    let mut class_properties: Vec<JSAstNode> = vec![];
    let mut transferred_state_variables: HashSet<TypedIdentifier> = HashSet::new();
    let mut class_methods: Vec<JSAstNode> = vec![];
    let mut endpoints: Vec<JSAstNode> = vec![];

    for stmts in slir {
        let state_transfer_index = stmts
            .iter()
            .position(|stmt| match stmt {
                SLIRNode::StateTransfer { .. } => true,
                _ => false,
            })
            .expect("At least one StateTransfer is necessary for tier splitting");
        let state_transfers: Vec<&StateTransfer> = stmts
            .iter()
            .skip(state_transfer_index)
            .map(|node| match node {
                SLIRNode::StateTransfer(st) => st,
                _ => panic!("All state transfer SLIRNodes must be of variant StateTransfer"),
            })
            .collect();
        let server_stmts: Vec<SLIRServerNode> = stmts
            .iter()
            .take(state_transfer_index)
            .map(|node| match node {
                SLIRNode::StateTransfer(..) => {
                    panic!("State transfers are not supported in the server tier")
                }
                SLIRNode::Logic(stmt) => SLIRServerNode::Logic(stmt.clone()),
                SLIRNode::StateQuery(sq) => SLIRServerNode::StateQuery(sq.clone()),
            })
            .collect();

        let mut methods =
            slir_make_state_transfers_client(&schema_name, &state_transfers, schemas, type_env);
        
        class_methods.append(&mut methods);

        for transfer in state_transfers {
            let endpoint = slir_expand_state_transfer_server(&transfer, &server_stmts, schemas);
            endpoints.push(endpoint);
            transferred_state_variables.insert(transfer.collection.clone());
        }
    }

    for state_var in transferred_state_variables {
        let class_property_iden = JSAstNode::TypedIdentifier {
            identifier: Box::new(JSAstNode::Identifier(
                state_var.identifier.name.clone(),
            )),
            r#type: Box::new(js_translate_type(&state_var.r#type)),
        };
        class_properties.push(JSAstNode::ClassProperty {typed_identifier: Box::new(class_property_iden) });
    }

    let config_func_type = format!("(a: {}) => void", schema_name);
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

    let mut expanded_definitions: Vec<JSAstNode> = vec![];
    for property in class_properties {
        expanded_definitions.push(property);
    }
    for method in class_methods {
        expanded_definitions.push(method);
    }

    expanded_definitions.insert(0, constructor);

    let client = JSAstNode::ClassDef {
        name: Box::new(JSAstNode::Identifier(schema_name.to_string())),
        body: Box::new(JSAstNode::ClassBody {
            definitions: expanded_definitions,
        }),
    };

    (client, endpoints)
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

// Display errors with line number:
//         let span = pair.as_span();
//         let (line, _) = span.start_pos().line_col();
//         println!(
//             "Error on line {}: {} refers to unknown type: {}",
//             line, attribute.name, attribute.r#type
//         )
//     }

type SLIROperations = Vec<SLIRNode>;

fn main() {
    let args = Args::parse();
    let source = std::fs::read_to_string(args.input_file).expect("No input file provided.");
    let result = LangParser::parse(Rule::Program, &source);

    // Mapping of names to Schema definitions
    let mut schemas: Schemas = HashMap::new();

    // Mapping of variables to their types
    let mut type_environment: TypeEnvironment = HashMap::new();

    // Function definitions need to be captured and placed on whichever tier
    // they are referenced by
    let mut function_defs: Vec<AstFunctionDef> = vec![];

    // Executable JS model representation of program
    let mut js_model: Vec<String> = vec![];

    // Expanded client representation of program
    let mut js_expanded_client_slir: Vec<String> = vec![];

    // Expanded server endpoints for infrastructure-expanded program
    let mut js_endpoints_slir: Vec<JSAstNode> = vec![];

    // Generated properties for certifying the semantic equivalence of
    // infrastructure-expanded program to source program (model)
    let mut certification_property_strs: Vec<String> = vec![];
    let mut certification_property_names: Vec<String> = vec![];

    // Setup seed types
    insert_seed_types(&mut type_environment);

    match result {
        Ok(pairs) => {
            for pair in pairs {
                let parsed = parse(pair.clone(), &schemas);
                println!("parsed: {:#?}", parsed);

                update_environment(
                    &pair,
                    &parsed,
                    &mut schemas,
                    &mut function_defs,
                    &mut type_environment,
                );

                let (slir, schema_name) = match &parsed {
                    AstNode::SchemaDef {
                        name: schema_name,
                        body,
                    } => (
                        body.into_iter()
                            .filter_map(|def| match def {
                                SchemaDefinition::SchemaMethod(SchemaMethod {
                                    name: method_name,
                                    args,
                                    body,
                                    ..
                                }) => Some(slir_translate(
                                    &schema_name.name,
                                    &method_name.name,
                                    &body,
                                    &schemas,
                                    &type_environment,
                                )),
                                _ => None,
                            })
                            .collect(),
                        schema_name.name.to_string(),
                    ),
                    _ => (vec![], "".to_string()),
                };

                println!("Tier splitting based on slir: {:#?}", slir);

                let (client_js, server_js) =
                    slir_tier_split(&schema_name, &parsed, &slir, &schemas, &type_environment);

//                println!("Client JS: {:#?}", client_js);
                js_expanded_client_slir.push(js_gen_string(client_js));


                for endpoint in server_js {
                    js_endpoints_slir.push(endpoint);
                }

                // js_translate is a direct translation, i.e. can contain conventions allowed in
                // Sligh that aren't allowed in JS. It is more of an intermediate representation.

                let js_ast = js_translate(parsed.clone());

                // println!("JS AST: {:#?}", js_ast);

                // js_executable_translate converts Sligh constructs to executable JS, e.g. by
                // replacing state transitions with array operations.
                // TODO: This also has to prepare model for testing, i.e. add an id param to create functions
                js_model.push(js_gen_string(js_executable_translate(&js_ast)));

                let actions = match parsed {
                    AstNode::SchemaDef { body, .. } => body
                        .into_iter()
                        .filter_map(|def| match def {
                            SchemaDefinition::SchemaMethod(m) => Some(m),
                            _ => None,
                        })
                        .collect(),
                    _ => vec![],
                };

                let (certification_properties, mut names) =
                    slir_gen_certification_properties(&actions, &schemas);

                for property in certification_properties {
                    certification_property_strs.push(js_gen_string(property));
                }
                certification_property_names.append(&mut names);
            }
        }
        Err(e) => println!("Error {:?}", e),
    }

//    let client_code = js_expanded_client.join("\n\n");
//    fs::write(args.client_output, client_code).expect("Unable to write client code file.");

    let client_code_slir = js_expanded_client_slir.join("\n\n");
    fs::write(args.client_output, client_code_slir).expect("Unable to write client code SLIR file.");

    let server_file = gen_server_file(&js_endpoints_slir, &function_defs);

    fs::write(args.server_output, server_file).expect("Unable to write server code SLIR file.");

    let model_code = js_model.join("\n\n");
    fs::write(args.model_output, model_code).expect("Unable to write model code file.");

    let certification_code = gen_certification_property_file(
        &certification_property_strs,
        &certification_property_names,
    );
    fs::write(
        "./certification-properties.ts",
        certification_code.join("\n\n"),
    )
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
