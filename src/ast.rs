#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Integer,
    Float,
    String,
    Boolean,
    Void,
    Custom(String), // For Interfaces, Classes, Enums
    Array(Box<Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(String, Type), // Value, Type
    Variable(String),
    Binary(Box<Expression>, String, Box<Expression>), // Left, Op, Right
    Call(Box<Expression>, Vec<Expression>), // Callee, Arguments
    Member(Box<Expression>, String), // obj.prop
    Index(Box<Expression>, Box<Expression>), // arr[i]
    ArrayLiteral(Vec<Expression>), // [1, 2, 3]
    StructLiteral(String, Vec<(String, Expression)>), // Name, Fields { name: val }
    BoolLiteral(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let {
        name: String,
        type_annotation: Option<Type>,
        initial_value: Option<Expression>,
        mutable: bool,
    },
    Assign {
        name: String,
        value: Expression,
    },
    Return(Option<Expression>),
    Expression(Expression),
    If {
        condition: Expression,
        then_branch: Vec<Statement>,
        else_branch: Option<Vec<Statement>>,
    },
    DoWhile {
        body: Vec<Statement>,
        condition: Expression,
    },
    MemberAssign {
        target: Expression, // e.g. this.type
        value: Expression,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub return_type: Type,
    pub body: Vec<Statement>,
    pub visibility: Visibility,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Interface {
    pub name: String,
    pub fields: Vec<(String, Type)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Enum {
    pub name: String,
    pub variants: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    pub name: String,
    pub fields: Vec<(String, Type)>,
    pub constructor: Option<Function>,
    pub methods: Vec<Function>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TopLevel {
    Function(Function),
    Interface(Interface),
    Enum(Enum),
    Class(Class),
    Statement(Statement), // For global statements like `person matheus := ...`
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub items: Vec<TopLevel>,
}
