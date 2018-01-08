use std::collections::HashMap;
use ast;
use ast::Ident;


struct Program
{
    funs: HashMap<Ident, Fun>,
    structs: HashMap<Ident, Struct>
}

struct Fun
{
    args: HashMap<Ident, usize>,
    body: Block,
    /* Corresponding to the needed size that the caller needs to reserve for
     * returning a struct (equels to 0 in another case)
     */
    ret_size: usize,
}

struct Struct
{
    size: usize,
    fields: HashMap<Ident, usize>
}

struct Block
{
    instr: Vec<Instr>,
    expr: Expr
}

enum Instr
{
    Expression(Expr),
    Let(usize, Expr),
    While(Expr, Box<Block>),
    Return(Expr),
}

enum Expr
{
    AssignLocal(Box<Expr>, Box<Expr>),

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