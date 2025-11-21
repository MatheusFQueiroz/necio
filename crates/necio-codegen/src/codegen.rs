use necio_parser::ast::{Class, Enum, Expression, Function, Interface, Program, Statement, TopLevel, Type, Visibility};
use std::collections::HashMap;

pub struct Codegen {
    program: Program,
    type_defs: HashMap<String, TypeDef>,
}

enum TypeDef {
    Interface(Interface),
    Class(Class),
    Enum(Enum),
}

impl Codegen {
    pub fn new(program: Program) -> Self {
        let mut type_defs = HashMap::new();
        for item in &program.items {
            match item {
                TopLevel::Interface(i) => { type_defs.insert(i.name.clone(), TypeDef::Interface(i.clone())); },
                TopLevel::Class(c) => { type_defs.insert(c.name.clone(), TypeDef::Class(c.clone())); },
                TopLevel::Enum(e) => { type_defs.insert(e.name.clone(), TypeDef::Enum(e.clone())); },
                _ => {}
            }
        }
        Self { program, type_defs }
    }

    pub fn generate(&self) -> String {
        let mut code = String::new();
        
        code.push_str("fn printl<T: std::fmt::Display>(val: T) -> T {\n");
        code.push_str("    println!(\"{}\", val);\n");
        code.push_str("    val\n");
        code.push_str("}\n\n");

        // Pass 1: Types and Functions
        for item in &self.program.items {
            match item {
                TopLevel::Function(func) => {
                    let is_main = func.name == "main";
                    let name = if is_main { "user_main" } else { &func.name };
                    let mut f = func.clone();
                    f.name = name.to_string();
                    code.push_str(&self.generate_function(&f, false));
                    code.push('\n');
                }
                TopLevel::Interface(iface) => {
                    code.push_str(&self.generate_interface(iface));
                    code.push('\n');
                }
                TopLevel::Enum(enm) => {
                    code.push_str(&self.generate_enum(enm));
                    code.push('\n');
                }
                TopLevel::Class(cls) => {
                    code.push_str(&self.generate_class(cls));
                    code.push('\n');
                }
                TopLevel::Statement(_) => {}
            }
        }
        
        // Pass 2: Main
        code.push_str("fn main() {\n");
        for item in &self.program.items {
            if let TopLevel::Statement(stmt) = item {
                code.push_str(&self.generate_statement(stmt));
            }
        }
        let has_main = self.program.items.iter().any(|i| matches!(i, TopLevel::Function(f) if f.name == "main"));
        if has_main {
            code.push_str("    user_main();\n");
        }
        code.push_str("}\n");

        code
    }

    fn escape_identifier(&self, name: &str) -> String {
        match name {
            "type" | "struct" | "enum" | "fn" | "let" | "match" | "if" | "else" | "while" | "loop" | "for" | "in" | "continue" | "break" | "return" | "pub" | "mod" | "use" | "crate" | "super" | "where" | "impl" | "trait" | "const" | "static" | "mut" | "unsafe" | "extern" | "ref" | "move" | "async" | "await" | "dyn" => format!("r#{}", name),
            _ => name.to_string(),
        }
    }

    fn to_pascal_case(&self, s: &str) -> String {
        let mut c = s.chars();
        match c.next() {
            None => String::new(),
            Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
        }
    }

    fn generate_interface(&self, iface: &Interface) -> String {
        let fields: Vec<String> = iface.fields.iter()
            .map(|(name, ty)| format!("    pub {}: {},", self.escape_identifier(name), self.generate_type(ty)))
            .collect();
        format!("#[derive(Debug, Clone)]\nstruct {} {{\n{}\n}}\n", self.to_pascal_case(&iface.name), fields.join("\n"))
    }

    fn generate_enum(&self, enm: &Enum) -> String {
        let variants: Vec<String> = enm.variants.iter()
            .map(|v| format!("    {},", self.escape_identifier(v)))
            .collect();
        format!("#[derive(Debug, Clone, PartialEq)]\nenum {} {{\n{}\n}}\n", self.to_pascal_case(&enm.name), variants.join("\n"))
    }

    fn generate_class(&self, cls: &Class) -> String {
        let fields: Vec<String> = cls.fields.iter()
            .map(|(name, ty)| format!("    pub {}: {},", self.escape_identifier(name), self.generate_type(ty)))
            .collect();
        let struct_def = format!("#[derive(Debug, Clone)]\nstruct {} {{\n{}\n}}\n", self.to_pascal_case(&cls.name), fields.join("\n"));
        
        let mut methods = String::new();
        if let Some(ctor) = &cls.constructor {
             methods.push_str(&self.generate_constructor(ctor, cls));
        }
        
        for method in &cls.methods {
            methods.push_str(&self.generate_function(method, true));
        }
        
        format!("{}\nimpl {} {{\n{}}}\n", struct_def, self.to_pascal_case(&cls.name), methods)
    }
    
    fn generate_constructor(&self, func: &Function, cls: &Class) -> String {
        let params: Vec<String> = func.params.iter()
            .map(|(name, ty)| format!("{}: {}", self.escape_identifier(name), self.generate_type(ty)))
            .collect();
            
        let init_fields: Vec<String> = cls.fields.iter()
            .map(|(name, ty)| format!("{}: {}", self.escape_identifier(name), self.generate_default_value(ty)))
            .collect();
            
        let mut body = String::new();
        body.push_str(&format!("        let mut self_ = {} {{\n            {}\n        }};\n", self.to_pascal_case(&cls.name), init_fields.join(",\n            ")));
        
        body.push_str("        let mut this_obj = self_;\n"); 
        
        for stmt in &func.body {
            body.push_str(&self.generate_statement(stmt));
        }
        body.push_str("        this_obj\n");
        
        format!("    pub fn new({}) -> Self {{\n        let mut this_obj = Self {{\n            {}\n        }};\n{}\n    }}\n", params.join(", "), init_fields.join(",\n            "), body)
    }
    
    fn generate_default_value(&self, ty: &Type) -> String {
        match ty {
            Type::Integer => "0".to_string(),
            Type::Float => "0.0".to_string(),
            Type::String => "String::new()".to_string(),
            Type::Boolean => "false".to_string(),
            Type::Void => "()".to_string(),
            Type::Array(_) => "Vec::new()".to_string(),
            Type::Custom(name) => {
                // Check if Enum or Struct
                if let Some(TypeDef::Enum(e)) = self.type_defs.get(name) {
                    if let Some(first) = e.variants.first() {
                        return format!("{}::{}", self.to_pascal_case(name), self.escape_identifier(first));
                    }
                } else if let Some(TypeDef::Class(c)) = self.type_defs.get(name) {
                    let fields: Vec<String> = c.fields.iter()
                        .map(|(f_name, f_ty)| format!("{}: {}", self.escape_identifier(f_name), self.generate_default_value(f_ty)))
                        .collect();
                    return format!("{} {{ {} }}", self.to_pascal_case(name), fields.join(", "));
                } else if let Some(TypeDef::Interface(i)) = self.type_defs.get(name) {
                    let fields: Vec<String> = i.fields.iter()
                        .map(|(f_name, f_ty)| format!("{}: {}", self.escape_identifier(f_name), self.generate_default_value(f_ty)))
                        .collect();
                    return format!("{} {{ {} }}", self.to_pascal_case(name), fields.join(", "));
                }
                "panic!(\"No default for custom type\")".to_string()
            },
        }
    }

    fn generate_function(&self, function: &Function, is_method: bool) -> String {
        let mut params: Vec<String> = function.params.iter()
            .map(|(name, ty)| format!("{}: {}", self.escape_identifier(name), self.generate_type(ty)))
            .collect();
        
        if is_method {
            params.insert(0, "&mut self".to_string());
        }
        
        let return_type = if function.return_type == Type::Void {
            "".to_string()
        } else {
            format!(" -> {}", self.generate_type(&function.return_type))
        };

        let mut body = String::new();
        if is_method {
             body.push_str("        let this_obj = self;\n");
        }
        
        for stmt in &function.body {
            body.push_str(&self.generate_statement(stmt));
        }

        let vis = match function.visibility {
            Visibility::Public => "pub ",
            Visibility::Private => "",
        };

        format!("    {}fn {}({}){} {{\n{}}}\n", vis, function.name, params.join(", "), return_type, body)
    }

    fn generate_type(&self, ty: &Type) -> String {
        match ty {
            Type::Integer => "i32".to_string(),
            Type::Float => "f64".to_string(),
            Type::String => "String".to_string(),
            Type::Boolean => "bool".to_string(),
            Type::Void => "()".to_string(),
            Type::Custom(name) => self.to_pascal_case(name),
            Type::Array(inner) => format!("Vec<{}>", self.generate_type(inner)),
        }
    }

    fn generate_statement(&self, stmt: &Statement) -> String {
        match stmt {
            Statement::Let { name, type_annotation, initial_value, mutable } => {
                let mut_kw = if *mutable { "mut " } else { "" };
                let type_str = if let Some(ty) = type_annotation {
                    format!(": {}", self.generate_type(ty))
                } else {
                    "".to_string()
                };
                let value = if let Some(expr) = initial_value {
                    format!(" = {}", self.generate_expression(expr, type_annotation.as_ref()))
                } else {
                    "".to_string()
                };
                format!("    let {}{}{}{};\n", mut_kw, self.escape_identifier(name), type_str, value)
            }
            Statement::Assign { name, value } => {
                format!("    {} = {};\n", self.escape_identifier(name), self.generate_expression(value, None))
            }
            Statement::MemberAssign { target, value } => {
                format!("    {} = {};\n", self.generate_expression(target, None), self.generate_expression(value, None))
            }
            Statement::Return(expr) => {
                if let Some(e) = expr {
                    format!("    return {};\n", self.generate_expression(e, None))
                } else {
                    "    return;\n".to_string()
                }
            }
            Statement::Expression(expr) => {
                format!("    {};\n", self.generate_expression(expr, None))
            }
            Statement::If { condition, then_branch, else_branch } => {
                let mut code = format!("    if {} {{\n", self.generate_expression(condition, None));
                for s in then_branch {
                    code.push_str(&self.generate_statement(s));
                }
                code.push_str("    }");
                if let Some(else_b) = else_branch {
                    code.push_str(" else {\n");
                    for s in else_b {
                        code.push_str(&self.generate_statement(s));
                    }
                    code.push_str("    }");
                }
                code.push_str("\n");
                code
            }
            Statement::DoWhile { body, condition } => {
                let mut code = "    loop {\n".to_string();
                for s in body {
                    code.push_str(&self.generate_statement(s));
                }
                code.push_str(&format!("        if !({}) {{ break; }}\n", self.generate_expression(condition, None)));
                code.push_str("    }\n");
                code
            }
        }
    }

    fn generate_expression(&self, expr: &Expression, expected_type: Option<&Type>) -> String {
        match expr {
            Expression::Literal(val, ty) => {
                match ty {
                    Type::String => format!("\"{}\".to_string()", val),
                    _ => val.clone(),
                }
            }
            Expression::BoolLiteral(b) => b.to_string(),
            Expression::Variable(name) => {
                if name == "self" { "this_obj".to_string() } else { self.escape_identifier(name) }
            },
            Expression::Binary(left, op, right) => {
                if op == "+" {
                    format!("{} {} &({})", self.generate_expression(left, None), op, self.generate_expression(right, None))
                } else {
                    format!("{} {} ({})", self.generate_expression(left, None), op, self.generate_expression(right, None))
                }
            }
            Expression::Call(callee, args) => {
                // Check if calling a Class Constructor
                if let Expression::Variable(name) = &**callee {
                    if let Some(TypeDef::Class(_)) = self.type_defs.get(name) {
                        let arg_strs: Vec<String> = args.iter().map(|a| self.generate_expression(a, None)).collect();
                        return format!("{}::new({})", self.to_pascal_case(name), arg_strs.join(", "));
                    }
                }
                
                let arg_strs: Vec<String> = args.iter().map(|a| self.generate_expression(a, None)).collect();
                let callee_str = self.generate_expression(callee, None);
                format!("{}({})", callee_str, arg_strs.join(", "))
            }
            Expression::Member(obj, prop) => {
                if let Expression::Variable(name) = &**obj {
                    if let Some(TypeDef::Enum(_)) = self.type_defs.get(name) {
                        return format!("{}::{}", self.to_pascal_case(name), self.escape_identifier(prop));
                    }
                }
                format!("{}.{}", self.generate_expression(obj, None), self.escape_identifier(prop))
            }
            Expression::Index(arr, idx) => {
                format!("{}[{} as usize]", self.generate_expression(arr, None), self.generate_expression(idx, None))
            }
            Expression::ArrayLiteral(elems) => {
                let elem_strs: Vec<String> = elems.iter().map(|e| self.generate_expression(e, None)).collect();
                format!("vec![{}]", elem_strs.join(", "))
            }
            Expression::StructLiteral(name, fields) => {
                let struct_name = if !name.is_empty() {
                    name.clone()
                } else if let Some(Type::Custom(n)) = expected_type {
                    n.clone()
                } else {
                    "UnknownStruct".to_string()
                };
                
                let mut field_types = HashMap::new();
                if let Some(TypeDef::Interface(i)) = self.type_defs.get(&struct_name) {
                    for (f_name, f_ty) in &i.fields {
                        field_types.insert(f_name.clone(), f_ty.clone());
                    }
                } else if let Some(TypeDef::Class(c)) = self.type_defs.get(&struct_name) {
                    for (f_name, f_ty) in &c.fields {
                        field_types.insert(f_name.clone(), f_ty.clone());
                    }
                }
                
                let field_strs: Vec<String> = fields.iter().map(|(f_name, f_val)| {
                    let f_ty = field_types.get(f_name);
                    format!("{}: {}", self.escape_identifier(f_name), self.generate_expression(f_val, f_ty))
                }).collect();
                
                format!("{} {{ {} }}", self.to_pascal_case(&struct_name), field_strs.join(", "))
            }
        }
    }
}
