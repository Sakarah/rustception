use std::collections::HashMap;
use std::collections::HashSet;
use ast;
use ast::Ident;
use symbol::Symbol;

pub enum Typ
{
    Struct(usize),
    Vector,
    Primitive
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
    pub ret_typ: Typ,
}

pub struct Struct
{
    pub size: usize,
    pub fields: HashMap<Ident, usize>
}

pub struct Block
{
    pub instr: Vec<Instr>,
    pub expr: Expr,
}

pub enum Instr
{
    Expression(Expr),
    Let(usize, Expr, Typ),
    While(Expr, Box<Block>),
    Return(Expr),
}

pub enum Expr
{
    AssignLocal(Box<Expr>, Box<Expr>, Typ),

    Logic(ast::LogicOp, Box<Expr>, Box<Expr>),
    Comparison(ast::Comp, Box<Expr>, Box<Expr>),
    Arithmetic(ast::ArithOp, Box<Expr>, Box<Expr>),

    Minus(Box<Expr>),
    Not(Box<Expr>),
    Ref(Box<Expr>),
    Deref(Box<Expr>),

    ArrayAccess(Box<Expr>, Box<Expr>),
    Attribute(Box<Expr>, usize),

    Constant(isize),
    Variable(isize),
    FunctionCall(Ident, Vec<Expr>),
    StructConstr(Vec<(Expr,usize)>),
    VecConstr(Vec<Expr>),
    VecLen(Box<Expr>),
    Print(Symbol),
    If(Box<Expr>, Box<Block>, Box<Block>),
    NestedBlock(Box<Block>)
}