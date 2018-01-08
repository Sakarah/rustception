use std::collections::HashMap;
use ast;
use ast::Ident;

enum Typ
{
    Struct(usize),
    Vector(usize),
    Primitive
}

struct Program
{
    funs: HashMap<Ident, Fun>,
    structs: HashMap<Ident, Struct>
}

struct Fun
{
    args: HashMap<Ident, usize>,
    body: Block,
}

struct Struct
{
    size: usize,
    fields: HashMap<Ident, usize>
}

struct Block
{
    instr: Vec<Instr>,
    expr: Expr,
    typ
}

enum Instr
{
    Expression(Expr),
    Let(usize, Expr, Typ),
    While(Expr, Box<Block>),
    Return(Expr),
}

enum Expr
{
    AssignLocal(Box<Expr>, Box<Expr>, Typ),

    Logic(ast::LogicOp, Box<Expr>, Box<Expr>),
    Comparison(ast::Comp, Box<Expr>, Box<Expr>),
    Arithmetic(ast::ArithOp, Box<Expr>, Box<Expr>),

    Minus(Box<Expr>),
    Not(Box<Expr>),
    Ref(Box<Expr>),
    Deref(Box<Expr>),
    MutRef(Box<Expr>),

    // Last argument of the ArrayAccess is the size of the arguments.
    ArrayAccess(Box<Expr>, Box<Expr>, usize),
    Attribute(Box<Expr>, usize),

    Constant(isize),
    Variable(isize),
    FunctionCall(Ident, Vec<Expr>),
    StructConstr(Vec<Expr>),
    VecConstr(Vec<Expr>),
    VecLen(Box<Expr>),
    Print(usize),
    If(Box<Expr>, Box<Block>, Box<Block>),
    NestedBlock(Box<Block>)
}