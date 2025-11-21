use crate::ast::{Class, Enum, Expression, Function, Interface, Program, Statement, TopLevel, Type, Visibility};
use crate::lexer::{Lexer, Token};

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let current_token = lexer.next_token();
        Self {
            lexer,
            current_token,
        }
    }

    fn eat(&mut self, _token_type: Token) {
        self.current_token = self.lexer.next_token();
    }

    fn match_token(&self, token: &Token) -> bool {
        std::mem::discriminant(&self.current_token) == std::mem::discriminant(token)
    }

    pub fn parse_program(&mut self) -> Program {
        let mut items = Vec::new();
        while self.current_token != Token::EOF {
            items.push(self.parse_top_level());
        }
        Program { items }
    }

    fn parse_top_level(&mut self) -> TopLevel {
        match self.current_token {
            Token::Interface => TopLevel::Interface(self.parse_interface()),
            Token::Enum => TopLevel::Enum(self.parse_enum()),
            Token::Class => TopLevel::Class(self.parse_class()),
            Token::Identifier(_) | Token::TypeInteger | Token::TypeString | Token::TypeFloat | Token::TypeBoolean | Token::TypeVoid => {
                if self.match_token(&Token::Let) {
                    TopLevel::Statement(self.parse_statement())
                } else {
                    // Try to parse as Type first
                    // But wait, if it is Identifier, it could be Type or Expression.
                    // If we parse as Type, we consume the Identifier.
                    // If it turns out to be Expression, we need to reconstruct.
                    
                    let ty = self.parse_type();
                    
                    if let Token::Identifier(name) = &self.current_token {
                        // Type ID ...
                        let name = name.clone();
                        self.eat(Token::Identifier("".to_string()));
                        
                        if self.match_token(&Token::LParen) {
                            // Function: Type ID ( ...
                            self.eat(Token::LParen);
                            let params = self.parse_params();
                            self.eat(Token::RParen);
                            self.eat(Token::LBrace);
                            let body = self.parse_block();
                            self.eat(Token::RBrace);
                            TopLevel::Function(Function {
                                name,
                                params,
                                return_type: ty,
                                body,
                                visibility: Visibility::Public,
                            })
                        } else if self.match_token(&Token::Assign) {
                            // Global variable: Type Name := Value
                            self.eat(Token::Assign);
                            let value = self.parse_expression();
                            self.eat(Token::SemiColon);
                            TopLevel::Statement(Statement::Let {
                                name,
                                type_annotation: Some(ty),
                                initial_value: Some(value),
                                mutable: false,
                            })
                        } else if self.match_token(&Token::Dot) {
                             // Type ID . ... -> This implies Type was Custom(ID).
                             // But we are here because next token was Identifier.
                             // Example: `matheus.friends...`
                             // `matheus` -> Type::Custom("matheus").
                             // Next is `.`. NOT Identifier.
                             // So we won't be in this block!
                             panic!("Unexpected token after Type ID: {:?}", self.current_token);
                        } else {
                             // Type ID ... -> Maybe `person matheus;` (no init)?
                             // Or `person matheus` followed by something else?
                             // For now, assume invalid or missing semicolon?
                             // Or maybe `Type ID` is expression? `Type` (ID) `ID` (Variable)?
                             // No.
                             panic!("Unexpected token after Type ID: {:?}", self.current_token);
                        }
                    } else {
                        // Type ... (Next is NOT Identifier)
                        // If Type was Custom(name), it could be an Expression starting with `name`.
                        // Example: `matheus.friends...`
                        // `matheus` parsed as Type::Custom("matheus").
                        // Next is `.`.
                        
                        if let Type::Custom(name) = ty {
                             let expr = self.parse_expression_continuation(Expression::Variable(name));
                             self.eat(Token::SemiColon);
                             TopLevel::Statement(Statement::Expression(expr))
                        } else {
                             // Primitive type followed by something else.
                             // `integer;` -> Invalid.
                             // `integer + 1;` -> Invalid.
                             panic!("Unexpected token after Type: {:?}", self.current_token);
                        }
                    }
                }
            }
            _ => panic!("Unexpected top level token: {:?}", self.current_token),
        }
    }

    fn parse_interface(&mut self) -> Interface {
        self.eat(Token::Interface);
        let name = match &self.current_token {
            Token::Identifier(s) => s.clone(),
            _ => panic!("Expected interface name"),
        };
        self.eat(Token::Identifier("".to_string()));
        self.eat(Token::LBrace);
        
        let mut fields = Vec::new();
        while !self.match_token(&Token::RBrace) {
            let field_name = match &self.current_token {
                Token::Identifier(s) => s.clone(),
                _ => panic!("Expected field name"),
            };
            self.eat(Token::Identifier("".to_string()));
            self.eat(Token::Colon);
            let field_type = self.parse_type();
            self.eat(Token::SemiColon);
            fields.push((field_name, field_type));
        }
        self.eat(Token::RBrace);
        
        Interface { name, fields }
    }

    fn parse_enum(&mut self) -> Enum {
        self.eat(Token::Enum);
        let name = match &self.current_token {
            Token::Identifier(s) => s.clone(),
            _ => panic!("Expected enum name"),
        };
        self.eat(Token::Identifier("".to_string()));
        self.eat(Token::LBrace);
        
        let mut variants = Vec::new();
        while !self.match_token(&Token::RBrace) {
            let variant = match &self.current_token {
                Token::Identifier(s) => s.clone(),
                _ => panic!("Expected variant name"),
            };
            self.eat(Token::Identifier("".to_string()));
            variants.push(variant);
            if self.match_token(&Token::Comma) {
                self.eat(Token::Comma);
            } else {
                break;
            }
        }
        self.eat(Token::RBrace);
        
        Enum { name, variants }
    }

    fn parse_class(&mut self) -> Class {
        self.eat(Token::Class);
        let name = match &self.current_token {
            Token::Identifier(s) => s.clone(),
            _ => panic!("Expected class name"),
        };
        self.eat(Token::Identifier("".to_string()));
        self.eat(Token::LBrace);
        
        let mut fields = Vec::new();
        let mut methods = Vec::new();
        let mut constructor = None;
        
        while !self.match_token(&Token::RBrace) {
            let visibility = if self.match_token(&Token::Public) {
                self.eat(Token::Public);
                Visibility::Public
            } else if self.match_token(&Token::Private) {
                self.eat(Token::Private);
                Visibility::Private
            } else {
                Visibility::Public
            };

            if let Token::Identifier(id) = &self.current_token {
                if id == &name {
                    // Constructor check: if next is (, it is constructor.
                    // If next is :, it is field.
                    // Since we can't peek, we consume ID and check next.
                    let id_val = id.clone();
                    self.eat(Token::Identifier("".to_string()));
                    
                    if self.match_token(&Token::LParen) {
                         // Constructor
                        self.eat(Token::LParen);
                        let params = self.parse_params();
                        self.eat(Token::RParen);
                        self.eat(Token::LBrace);
                        let body = self.parse_block();
                        self.eat(Token::RBrace);
                        
                        constructor = Some(Function {
                            name: "new".to_string(),
                            params,
                            return_type: Type::Custom(name.clone()),
                            body,
                            visibility,
                        });
                        continue;
                    } else if self.match_token(&Token::Colon) {
                        // Field
                        self.eat(Token::Colon);
                        let field_type = self.parse_type();
                        self.eat(Token::SemiColon);
                        fields.push((id_val, field_type));
                        continue;
                    } else {
                         // Could be method if ID was Type? No, ID is ID.
                         // If ID is Type, then next is Method Name.
                         // But here ID matched Class Name.
                         // So it's either Constructor or Field of type ClassName.
                         // Field syntax: Name: Type.
                         // If we parsed Name, next is Colon.
                         // If we parsed Type, next is Name.
                         // Here we assumed it's Name.
                         panic!("Unexpected token in class body after identifier: {:?}", self.current_token);
                    }
                }
            }

            let ty_or_name = self.parse_type();
            
            if self.match_token(&Token::Colon) {
                if let Type::Custom(field_name) = ty_or_name {
                    self.eat(Token::Colon);
                    let field_type = self.parse_type();
                    self.eat(Token::SemiColon);
                    fields.push((field_name, field_type));
                } else {
                    panic!("Expected field name identifier");
                }
            } else if let Token::Identifier(method_name) = &self.current_token {
                let method_name = method_name.clone();
                self.eat(Token::Identifier("".to_string()));
                self.eat(Token::LParen);
                let params = self.parse_params();
                self.eat(Token::RParen);
                self.eat(Token::LBrace);
                let body = self.parse_block();
                self.eat(Token::RBrace);
                
                methods.push(Function {
                    name: method_name,
                    params,
                    return_type: ty_or_name,
                    body,
                    visibility,
                });
            } else {
                panic!("Unexpected token in class body: {:?}", self.current_token);
            }
        }
        self.eat(Token::RBrace);
        
        Class { name, fields, constructor, methods }
    }

    fn parse_type(&mut self) -> Type {
        match &self.current_token {
            Token::TypeInteger => { self.eat(Token::TypeInteger); Type::Integer },
            Token::TypeString => { self.eat(Token::TypeString); Type::String },
            Token::TypeFloat => { self.eat(Token::TypeFloat); Type::Float },
            Token::TypeBoolean => { self.eat(Token::TypeBoolean); Type::Boolean },
            Token::TypeVoid => { self.eat(Token::TypeVoid); Type::Void },
            Token::Identifier(s) => {
                let name = s.clone();
                self.eat(Token::Identifier("".to_string()));
                Type::Custom(name)
            },
            Token::LT => {
                self.eat(Token::LT);
                let inner = self.parse_type();
                self.eat(Token::GT);
                
                if self.match_token(&Token::LBracket) {
                    self.eat(Token::LBracket);
                    self.eat(Token::RBracket);
                    Type::Array(Box::new(inner))
                } else {
                    inner
                }
            },
            _ => panic!("Expected type, got {:?}", self.current_token),
        }
    }

    fn parse_params(&mut self) -> Vec<(String, Type)> {
        let mut params = Vec::new();
        if self.match_token(&Token::RParen) {
            return params;
        }
        loop {
            let name = match &self.current_token {
                Token::Identifier(s) => s.clone(),
                _ => panic!("Expected parameter name"),
            };
            self.eat(Token::Identifier("".to_string()));
            self.eat(Token::Colon);
            let param_type = self.parse_type();
            params.push((name, param_type));
            if self.match_token(&Token::Comma) {
                self.eat(Token::Comma);
            } else {
                break;
            }
        }
        params
    }

    fn parse_block(&mut self) -> Vec<Statement> {
        let mut statements = Vec::new();
        while !self.match_token(&Token::RBrace) && !self.match_token(&Token::EOF) {
            statements.push(self.parse_statement());
        }
        statements
    }

    fn parse_statement(&mut self) -> Statement {
        match &self.current_token {
            Token::Let => self.parse_let(),
            Token::If => self.parse_if(),
            Token::Do => self.parse_do_while(),
            Token::Return => self.parse_return(),
            Token::Identifier(_) | Token::This => self.parse_expr_stmt(),
            _ => panic!("Unexpected token in statement: {:?}", self.current_token),
        }
    }

    fn parse_let(&mut self) -> Statement {
        self.eat(Token::Let);
        let mut mutable = false;
        if self.match_token(&Token::Star) {
            self.eat(Token::Star);
            mutable = true;
        }
        let name = match &self.current_token {
            Token::Identifier(s) => s.clone(),
            _ => panic!("Expected identifier"),
        };
        self.eat(Token::Identifier("".to_string()));
        self.eat(Token::Colon);
        let ty = self.parse_type();
        
        let mut init = None;
        if self.match_token(&Token::Assign) {
            self.eat(Token::Assign);
            init = Some(self.parse_expression());
        }
        self.eat(Token::SemiColon);
        Statement::Let { name, type_annotation: Some(ty), initial_value: init, mutable }
    }

    fn parse_if(&mut self) -> Statement {
        self.eat(Token::If);
        self.eat(Token::LParen);
        let condition = self.parse_expression();
        self.eat(Token::RParen);
        self.eat(Token::LBrace);
        let then_branch = self.parse_block();
        self.eat(Token::RBrace);
        
        let mut else_branch = None;
        if self.match_token(&Token::Else) {
            self.eat(Token::Else);
            if self.match_token(&Token::If) {
                 else_branch = Some(vec![self.parse_if()]);
            } else {
                self.eat(Token::LBrace);
                else_branch = Some(self.parse_block());
                self.eat(Token::RBrace);
            }
        }
        Statement::If { condition, then_branch, else_branch }
    }

    fn parse_do_while(&mut self) -> Statement {
        self.eat(Token::Do);
        self.eat(Token::LBrace);
        let body = self.parse_block();
        self.eat(Token::RBrace);
        self.eat(Token::While);
        self.eat(Token::LParen);
        let condition = self.parse_expression();
        self.eat(Token::RParen);
        self.eat(Token::SemiColon);
        Statement::DoWhile { body, condition }
    }

    fn parse_return(&mut self) -> Statement {
        self.eat(Token::Return);
        let mut expr = None;
        if !self.match_token(&Token::SemiColon) {
            expr = Some(self.parse_expression());
        }
        self.eat(Token::SemiColon);
        Statement::Return(expr)
    }

    fn parse_expr_stmt(&mut self) -> Statement {
        let expr = self.parse_expression();
        
        if self.match_token(&Token::Assign) {
            self.eat(Token::Assign);
            let value = self.parse_expression();
            self.eat(Token::SemiColon);
            
            match expr {
                Expression::Variable(name) => Statement::Assign { name, value },
                Expression::Member(obj, prop) => Statement::MemberAssign { target: Expression::Member(obj, prop), value },
                _ => panic!("Invalid assignment target"),
            }
        } else {
            self.eat(Token::SemiColon);
            Statement::Expression(expr)
        }
    }

    fn parse_expression(&mut self) -> Expression {
        self.parse_binary()
    }

    fn parse_binary(&mut self) -> Expression {
        let mut left = self.parse_primary();
        
        while self.match_token(&Token::Plus) || self.match_token(&Token::LT) || self.match_token(&Token::GT) || self.match_token(&Token::EqEq) {
            let op = match self.current_token {
                Token::Plus => "+",
                Token::LT => "<",
                Token::GT => ">",
                Token::EqEq => "==",
                _ => unreachable!(),
            }.to_string();
            self.eat(self.current_token.clone());
            let right = self.parse_primary();
            left = Expression::Binary(Box::new(left), op, Box::new(right));
        }
        left
    }

    fn parse_primary(&mut self) -> Expression {
        let mut expr = match &self.current_token {
            Token::NumberLiteral(n) => {
                let val = n.clone();
                self.eat(Token::NumberLiteral("".to_string()));
                Expression::Literal(val, Type::Integer)
            },
            Token::FloatLiteral(n) => {
                let val = n.clone();
                self.eat(Token::FloatLiteral("".to_string()));
                Expression::Literal(val, Type::Float)
            },
            Token::StringLiteral(s) => {
                let val = s.clone();
                self.eat(Token::StringLiteral("".to_string()));
                Expression::Literal(val, Type::String)
            },
            Token::True => { self.eat(Token::True); Expression::BoolLiteral(true) },
            Token::False => { self.eat(Token::False); Expression::BoolLiteral(false) },
            Token::Identifier(s) => {
                let name = s.clone();
                self.eat(Token::Identifier("".to_string()));
                if self.match_token(&Token::LParen) {
                    self.eat(Token::LParen);
                    let args = self.parse_args();
                    self.eat(Token::RParen);
                    Expression::Call(Box::new(Expression::Variable(name)), args)
                } else {
                    Expression::Variable(name)
                }
            },
            Token::This => {
                self.eat(Token::This);
                Expression::Variable("self".to_string())
            },
            Token::LBrace => {
                self.eat(Token::LBrace);
                let mut fields = Vec::new();
                while !self.match_token(&Token::RBrace) {
                    let name = match &self.current_token {
                        Token::Identifier(s) => s.clone(),
                        _ => panic!("Expected field name"),
                    };
                    self.eat(Token::Identifier("".to_string()));
                    self.eat(Token::Colon);
                    let val = self.parse_expression();
                    fields.push((name, val));
                    if self.match_token(&Token::Comma) {
                        self.eat(Token::Comma);
                    } else {
                        break;
                    }
                }
                self.eat(Token::RBrace);
                Expression::StructLiteral("".to_string(), fields)
            },
            Token::LBracket => {
                self.eat(Token::LBracket);
                let mut elems = Vec::new();
                while !self.match_token(&Token::RBracket) {
                    elems.push(self.parse_expression());
                    if self.match_token(&Token::Comma) {
                        self.eat(Token::Comma);
                    } else {
                        break;
                    }
                }
                self.eat(Token::RBracket);
                Expression::ArrayLiteral(elems)
            },
            _ => panic!("Unexpected token in expression: {:?}", self.current_token),
        };

        loop {
            if self.match_token(&Token::Dot) {
                self.eat(Token::Dot);
                let prop = match &self.current_token {
                    Token::Identifier(s) => s.clone(),
                    _ => panic!("Expected property name"),
                };
                self.eat(Token::Identifier("".to_string()));
                
                if self.match_token(&Token::LParen) {
                    // Method call: obj.method(...)
                    // We represent this as Call(Member(obj, method), args)
                    // This requires Codegen to handle Call(Member) specially.
                    self.eat(Token::LParen);
                    let args = self.parse_args();
                    self.eat(Token::RParen);
                    expr = Expression::Call(Box::new(Expression::Member(Box::new(expr), prop)), args);
                } else {
                    expr = Expression::Member(Box::new(expr), prop);
                }
            } else if self.match_token(&Token::LBracket) {
                self.eat(Token::LBracket);
                let index = self.parse_expression();
                self.eat(Token::RBracket);
                expr = Expression::Index(Box::new(expr), Box::new(index));
            } else if self.match_token(&Token::Increment) {
                self.eat(Token::Increment);
                expr = Expression::Binary(Box::new(expr), "+".to_string(), Box::new(Expression::Literal("1".to_string(), Type::Integer)));
            } else {
                break;
            }
        }
        expr
    }
    
    fn parse_expression_continuation(&mut self, mut expr: Expression) -> Expression {
        loop {
            if self.match_token(&Token::Dot) {
                self.eat(Token::Dot);
                let prop = match &self.current_token {
                    Token::Identifier(s) => s.clone(),
                    _ => panic!("Expected property name"),
                };
                self.eat(Token::Identifier("".to_string()));
                if self.match_token(&Token::LParen) {
                    self.eat(Token::LParen);
                    let args = self.parse_args();
                    self.eat(Token::RParen);
                    expr = Expression::Call(Box::new(Expression::Member(Box::new(expr), prop)), args);
                } else {
                    expr = Expression::Member(Box::new(expr), prop);
                }
            } else if self.match_token(&Token::LParen) {
                self.eat(Token::LParen);
                let args = self.parse_args();
                self.eat(Token::RParen);
                expr = Expression::Call(Box::new(expr), args);
            } else if self.match_token(&Token::LBracket) {
                self.eat(Token::LBracket);
                let index = self.parse_expression();
                self.eat(Token::RBracket);
                expr = Expression::Index(Box::new(expr), Box::new(index));
            } else if self.match_token(&Token::Increment) {
                self.eat(Token::Increment);
                expr = Expression::Binary(Box::new(expr), "+".to_string(), Box::new(Expression::Literal("1".to_string(), Type::Integer)));
            } else {
                break;
            }
        }
        expr
    }

    fn parse_args(&mut self) -> Vec<Expression> {
        let mut args = Vec::new();
        if self.match_token(&Token::RParen) {
            return args;
        }
        loop {
            args.push(self.parse_expression());
            if self.match_token(&Token::Comma) {
                self.eat(Token::Comma);
            } else {
                break;
            }
        }
        args
    }
}
