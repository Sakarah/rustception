use location::{Span,Located};
use ast;
use ast::{Ident,LIdent,Type,LType};
use std::collections::HashMap;

pub struct Typed<T>
{
    pub data: T,
    pub typ: Type,
    pub mutable: bool,
    pub lvalue: bool,
    pub loc: Span
}

pub struct Program
{
    pub funs: HashMap<Ident, Fun>,
    pub structs: HashMap<Ident, Struct>
}

#[derive(Clone)]
pub struct FunSignature
{
    pub arguments: Vec<ast::Arg>,
    pub return_type: LType
}

pub struct Fun
{
    pub sig: FunSignature,
    pub body: Block
}

pub struct Struct
{
    pub fields: HashMap<Ident, LType>
}

pub struct Block
{
    pub instr: Vec<LInstr>,
    pub expr: TExpr
}

pub type LInstr = Located<Instr>;
pub enum Instr
{
    Expression(TExpr),
    Let(LIdent, TExpr),
    While(TExpr, Box<Block>),
    Return(TExpr)
}

pub type TExpr = Typed<Expr>;
pub enum Expr
{
    Assignment(Box<TExpr>, Box<TExpr>),

    Logic(ast::LogicOp, Box<TExpr>, Box<TExpr>),
    Comparison(ast::Comp, Box<TExpr>, Box<TExpr>),
    Arithmetic(ast::ArithOp, Box<TExpr>, Box<TExpr>),

    Minus(Box<TExpr>),
    Not(Box<TExpr>),
    Deref(Box<TExpr>),
    Ref(Box<TExpr>),
    MutRef(Box<TExpr>),

    ArrayAccess(Box<TExpr>, Box<TExpr>),
    Attribute(Box<TExpr>, LIdent),
    MethodCall(Box<TExpr>, LIdent, Vec<TExpr>),

    Constant(ast::Const),
    Variable(LIdent),
    FunctionCall(LIdent, Vec<TExpr>),
    StructConstr(LIdent, Vec<(LIdent, TExpr)>),
    VecConstr(Vec<TExpr>),
    Print(String),
    If(Box<TExpr>, Box<Block>, Box<Block>),
    NestedBlock(Box<Block>)
}

