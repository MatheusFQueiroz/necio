# Necio Programming Language

A statically-typed programming language that transpiles to Rust, featuring interfaces, classes, enums, and Rust-like mutability semantics.

## Architecture

Necio follows a modular workspace architecture with separate crates for different compiler phases:

```
necio/
├── deps/                 # Project dependencies
├── crates/
│   ├── necio-cli/        # Command-line interface
│   ├── necio-parser/     # Lexer, Parser, and AST
│   ├── necio-codegen/    # Rust code generation
│   ├── necio-analysis/   # Future: Type checking
│   └── necio-stdlib/     # Future: Standard library
└── examples/             # Example Necio programs
```

## Building

Build all crates in the workspace:

```bash
cargo build --workspace
```

## Usage

Compile Necio program to Rust:

```bash
cargo run --bin necio -- <input.necio> <output.rs>
```

Example:

```bash
# Compile Necio to Rust
cargo run --bin necio -- examples/main.necio output/output.rs

# Compile and run the generated Rust code
cd output
rustc output.rs -o output.exe
./output
```

## License

MIT
