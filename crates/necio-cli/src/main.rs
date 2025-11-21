use necio_parser::{Lexer, Parser};
use necio_codegen::Codegen;
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    let input_file = if args.len() > 1 {
        &args[1]
    } else {
        "main.necio"
    };
    
    let output_file = if args.len() > 2 {
        &args[2]
    } else {
        "output.rs"
    };
    
    // Read input file
    let input = fs::read_to_string(input_file)
        .unwrap_or_else(|_| panic!("Failed to read file: {}", input_file));
    
    // Lex and parse
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    
    // Generate Rust code
    let codegen = Codegen::new(program);
    let rust_code = codegen.generate();
    
    // Write output file
    fs::write(output_file, rust_code)
        .unwrap_or_else(|_| panic!("Failed to write file: {}", output_file));
    
    println!("Successfully compiled {} to {}", input_file, output_file);
}
