use symbol::Symbol;
use location::{Span,Located};
use ast;
use ast::{Ident,LIdent};
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt;

pub struct Typed<T>
{
    pub data: T,
    pub typ: Type,
    pub mutable: bool,
    pub lvalue: bool,
    pub always_return: bool,
    pub loc: Span
}

pub struct Program
{
    pub fun_sigs: HashMap<Ident, FunSignature>,
    pub fun_bodies: HashMap<Ident, Block>,
    pub structs: HashMap<Ident, Struct>
}

pub struct FunSignature
{
    pub arguments: Vec<Arg>,
    pub return_type: LType
}

pub struct Struct
{
    pub fields: HashMap<Ident, LType>
}

pub type LType = Located<Type>;
// The Type enum only stores well formed types
#[derive(Clone, Debug)]
pub enum Type
{
    Void,
    Int32,
    Bool,
    Struct(Ident),
    Vector(Box<Type>),
    Ref(Box<Type>),
    MutRef(Box<Type>),
    Unknown(Rc<RefCell<Option<Type>>>) // Types that will be discovered later
}

impl fmt::Display for Type
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match *self
        {
            Type::Void => write!(f, "()"),
            Type::Int32 => write!(f, "i32"),
            Type::Bool => write!(f, "bool"),
            Type::Struct(ref s) => write!(f, "struct {}", s),
            Type::Vector(ref t) => write!(f, "Vec<{}>", t),
            Type::Ref(ref t) => write!(f, "&{}", t),
            Type::MutRef(ref t) => write!(f, "&mut {}", t),
            Type::Unknown(ref t) =>
            {
                match *t.borrow()
                {
                    Some(ref t) => write!(f, "{}", t),
                    None => write!(f, "_")
                }
            }
        }
    }
}

#[derive(Clone)]
pub struct Arg
{
    pub mutable: bool,
    pub name: LIdent,
    pub typ: LType
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
    Constant(ast::Const),
    Variable(LIdent),
    Assignment(Box<TExpr>, Box<TExpr>),
    FunctionCall(LIdent, Vec<TExpr>),

    Logic(ast::LogicOp, Box<TExpr>, Box<TExpr>),
    Comparison(ast::Comp, Box<TExpr>, Box<TExpr>),
    Arithmetic(ast::ArithOp, Box<TExpr>, Box<TExpr>),
    Minus(Box<TExpr>),
    Not(Box<TExpr>),

    Deref(Box<TExpr>),
    Ref(Box<TExpr>),
    MutRef(Box<TExpr>),

    StructConstr(LIdent, Vec<(LIdent, TExpr)>),
    Attribute(Box<TExpr>, LIdent),

    VecConstr(Vec<TExpr>),
    ArrayAccess(Box<TExpr>, Box<TExpr>),
    VecLen(Box<TExpr>),

    Print(Symbol),

    If(Box<TExpr>, Box<Block>, Box<Block>),
    NestedBlock(Box<Block>)
}

