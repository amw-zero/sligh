use pest::{self, Parser};

#[derive(pest_derive::Parser)]
#[grammar = "grammar.pest"]
struct LangParser;

fn main() {
    let source = std::fs::read_to_string("./src/test.lang").expect("Gotta exist");
    let result = LangParser::parse(Rule::Program, &source);
    match result {
        Ok(pairs) => {
            for pair in pairs {
                println!("{:?}", pair)
            }
        },
        Err(e) => println!("Error {:?}", e)
    }
}
