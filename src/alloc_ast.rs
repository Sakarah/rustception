use std::collections::HashMap;
use std::collections::HashSet;
use ast;
use ast::Ident;
use symbol::Symbol;

#[derive(Clone, Copy)]
pub enum Typ
{
    Primitive,
    Vector,
    Struct(usize),
}

pub struct Program
{
    pub funs: HashMap<Ident, Fun>,
    pub structs: HashMap<Ident, Struct>,
    pub strings: HashSet<Symbol>,
}

pub struct Fun
{
    pub args: HashMap<Ident, usize>,
    pub body: Block,
    pub body_stack_size: usize,
    pub args_size: usize,
}

pub struct Struct
{
    pub size: usize,
    pub fields: HashMap<Ident, usize>
}

pub struct Block
{
    pub instr: Vec<Instr>,
    pub expr: TExpr,
}

pub enum Instr
{
    Expression(TExpr),
    Let(isize, TExpr),
    While(TExpr, Box<Block>),
    Return(TExpr),
}

pub struct TExpr
{
    pub data: Expr,
    pub typ: Typ,
    pub lvalue: bool
}

pub enum Expr
{
    Constant(isize),
    Variable(isize),
    AssignLocal(Box<TExpr>, Box<TExpr>),
    FunctionCall(Ident, Vec<TExpr>),

    Logic(ast::LogicOp, Box<TExpr>, Box<TExpr>),
    Comparison(ast::Comp, Box<TExpr>, Box<TExpr>),
    Arithmetic(ast::ArithOp, Box<TExpr>, Box<TExpr>),
    Minus(Box<TExpr>),
    Not(Box<TExpr>),

    Ref(Box<TExpr>),
    Deref(Box<TExpr>),

    StructConstr(Vec<(TExpr,usize)>), // usize = field offset
    Attribute(Box<TExpr>, usize), // struct_expr, field_off

    VecConstr(Vec<TExpr>),
    ArrayAccess(Box<TExpr>, Box<TExpr>),
    VecLen(Box<TExpr>),

    Print(Symbol),

    If(Box<TExpr>, Box<Block>, Box<Block>),
    NestedBlock(Box<Block>)
}
