use std::fs;
use necio::lexer::Lexer;
use necio::parser::Parser;
use necio::codegen::Codegen;

fn main() {
    let input = fs::read_to_string("main.necio").expect("Unable to read file");
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    let codegen = Codegen::new(program);
    let rust_code = codegen.generate();

    fs::write("output.rs", rust_code).expect("Unable to write output file");
    println!("Successfully compiled to output.rs");
}
