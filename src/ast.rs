use location::Located;

pub type Ident = String;
pub type LIdent = Located<Ident>;

pub type Program = Vec<Decl>;

pub enum Decl
{
    Function(Fun),
    Structure(Struct)
}

pub struct Fun // fn <Ident>(<Arg>,*) -> <Type> <Block>
{
    pub name: LIdent,
    pub arguments: Vec<Arg>,
    pub return_type: LType,
    pub body: Block
}

pub struct Struct // struct <Ident> { (<Ident> : <Type>),* }
{
    pub name: LIdent,
    pub fields: Vec<Field>
}

pub struct Field
{
    pub name: LIdent,
    pub typ: LType
}

pub type LType = Located<Type>;
#[derive(Clone, Debug)]
pub enum Type
{
    Void,                            // ()
    Basic(Ident),                    // <Ident>
    Parametrized(Ident, Box<Type>),  // <Ident>'<'<Type>'>'
    Ref(Box<Type>),                  // &<Type>
    MutRef(Box<Type>)                // &mut <Type>
}

pub struct Arg // "mut"? <Ident> : <Type>
{
    pub mutable: bool,
    pub name: LIdent,
    pub typ: LType
}

pub struct Block // { <Instr>* <Expr>? }
{
    pub instr: Vec<LInstr>,
    pub expr: LExpr
}

pub type LInstr = Located<Instr>;
pub enum Instr
{
    NoOp,                     // ;
    Expression(LExpr),        // <Expr>;
    Let(bool, LIdent, LExpr), // let "mut"? <Ident> = <Expr>;
    While(LExpr, Box<Block>), // while <Expr> <Block>
    Return(LExpr),            // return <Expr>?;
    If(IfExpr)                // <IfExpr>
}

pub enum IfExpr
{
    Single(LExpr, Box<Block>, Box<Block>), // if <Expr> <Block> (else <Block>)?
    Nested(LExpr, Box<Block>, Box<IfExpr>) // if <Expr> <Block> else <IfExpr>
}

pub type LExpr = Located<Expr>;
pub enum Expr
{
    Assignment(Box<LExpr>, Box<LExpr>),         // <Expr> = <Expr>

    Logic(LogicOp, Box<LExpr>, Box<LExpr>),     // <Expr> <LogicOp> <Expr>
    Comparison(Comp, Box<LExpr>, Box<LExpr>),   // <Expr> <Comp> <Expr>
    Arithmetic(ArithOp, Box<LExpr>, Box<LExpr>),// <Expr> <ArithOp> <Expr>

    Minus(Box<LExpr>),                          // - <Expr>
    Not(Box<LExpr>),                            // ! <Expr>
    Deref(Box<LExpr>),                          // * <Expr>
    Ref(Box<LExpr>),                            // & <Expr>
    MutRef(Box<LExpr>),                         // & mut <Expr>

    ArrayAccess(Box<LExpr>, Box<LExpr>),        // <Expr>[<Expr>]
    Attribute(Box<LExpr>, LIdent),              // <Expr>.<Ident>
    MethodCall(Box<LExpr>, LIdent, Vec<LExpr>), // <Expr>.<Ident>(<Expr>,*)

    Constant(Const),                            // <Const>
    Variable(LIdent),                           // <Ident>
    FunctionCall(LIdent, Vec<LExpr>),           // <Ident>(<Expr>,*)
    StructConstr(LIdent, Vec<(LIdent, LExpr)>), // <Ident>{ (<Ident>:<Expr>),* }
    ListMacro(LIdent,Vec<LExpr>),               // <Ident>![<Expr>,*]
    StringMacro(LIdent,String),                 // <Ident>!("<StringLiteral>")
    If(Box<IfExpr>),                            // <IfExpr>
    NestedBlock(Box<Block>)                     // <Block>
}

#[derive(Clone)]
pub enum Const
{
    Void,
    Int32(i32),
    Bool(bool)
}

#[derive(Clone, Copy)]
pub enum LogicOp
{
    Or, // ||
    And // &&
}

#[derive(Clone, Copy)]
pub enum Comp
{
    Equal,      // ==
    NotEqual,   // !=
    Less,       // <
    LessEq,     // <=
    Greater,    // >
    GreaterEq,  // >=
}

#[derive(Clone, Copy)]
pub enum ArithOp
{
    Addition,       // +
    Substraction,   // -
    Multiplication, // *
    Division,       // /
    Remainder       // %
}

pub const EXPR_VOID : Expr = Expr::Constant(Const::Void);
pub const LEXPR_VOID : LExpr =
    Located { data:EXPR_VOID, loc: ::location::EMPTY_SPAN };
