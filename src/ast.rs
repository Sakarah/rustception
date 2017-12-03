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

#[derive(Clone)]
pub struct Struct // struct <Ident> { (<Ident> : <Type>),* }
{
    pub name: LIdent,
    pub fields: Vec<(LIdent, LType)>
}

pub type LType = Located<Type>;

#[derive(Clone)]
pub enum Type
{
    Void,                            // ()
    Basic(LIdent),                   // <Ident>
    Parametrized(LIdent,Box<LType>), // <Ident>'<'<Type>'>'
    Ref(Box<LType>),                 // &<Type>
    MutRef(Box<LType>)               // &mut <Type>
}

#[derive(Clone, Copy)]
pub enum Mut
{
    Constant,   // ε
    Mutable     // mut
}

pub struct Arg // <Mut> <Ident> : <Type>
{
    pub m: Mut,
    pub name: LIdent,
    pub typ: LType
}

#[derive(Clone)]
pub struct Block(pub Vec<LInstr>, pub LExpr); // { <Instr>* <Expr>? }

pub type LInstr = Located<Instr>;
#[derive(Clone)]
pub enum Instr
{
    NoOp,                     // ;
    Expression(LExpr),        // <Expr>;
    Let(Mut, LIdent, LExpr),  // let <Mut> <Ident> = <Expr>;
    While(LExpr, Box<Block>), // while <Expr> <Block>
    Return(LExpr),            // return <Expr>?;
    If(IfExpr)                // <IfExpr>
}

#[derive(Clone)]
pub enum IfExpr
{
    Single(LExpr, Box<Block>, Box<Block>), // if <Expr> <Block> (else <Block>)?
    Nested(LExpr, Box<Block>, Box<IfExpr>) // if <Expr> <Block> else <IfExpr>
}

pub type LExpr = Located<Expr>;
#[derive(Clone)]
pub enum Expr
{
    Assignment(Box<LExpr>, Box<LExpr>),        // <Expr> = <Expr>
    Or(Box<LExpr>, Box<LExpr>),                // <Expr> || <Expr>
    And(Box<LExpr>, Box<LExpr>),               // <Expr> && <Expr>
    Comparison(Comp, Box<LExpr>, Box<LExpr>),  // <Expr> <Comp> <Expr>

    Addition(Box<LExpr>, Box<LExpr>),          // <Expr> + <Expr>
    Substraction(Box<LExpr>, Box<LExpr>),      // <Expr> - <Expr>
    Multiplication(Box<LExpr>, Box<LExpr>),    // <Expr> * <Expr>
    Division(Box<LExpr>, Box<LExpr>),          // <Expr> / <Expr>
    Remainder(Box<LExpr>, Box<LExpr>),         // <Expr> % <Expr>

    Minus(Box<LExpr>),                         // - <Expr>
    Not(Box<LExpr>),                           // ! <Expr>
    Deref(Box<LExpr>),                         // * <Expr>
    Ref(Box<LExpr>),                           // & <Expr>
    MutRef(Box<LExpr>),                        // & mut <Expr>

    ArrayAccess(Box<LExpr>, Box<LExpr>),       // <Expr>[<Expr>]
    Attribute(Box<LExpr>, LIdent),             // <Expr>.<Ident>
    MethodCall(Box<LExpr>, LIdent, Vec<LExpr>),// <Expr>.<Ident>(<Expr>,*)

    Constant(Const),                           // <Const>
    Variable(LIdent),                          // <Ident>
    FunctionCall(LIdent, Vec<LExpr>),          // <Ident>(<Expr>,*)
    StructConstr(LIdent, Vec<(LIdent, LExpr)>),// <Ident>{ (<Ident>:<Expr>),* }
    ListMacro(LIdent,Vec<LExpr>),              // <Ident>![<Expr>,*]
    StringMacro(LIdent,String),                // <Ident>!("<StringLiteral>")
    If(Box<IfExpr>),                           // <IfExpr>
    NestedBlock(Box<Block>)                    // <Block>
}

#[derive(Clone)]
pub enum Const
{
    Void,
    Int32(i32),
    Bool(bool)
}

#[derive(Clone)]
pub enum Comp
{
    Equal,      // ==
    NotEqual,   // !=
    Less,       // <
    LessEq,     // <=
    Greater,    // >
    GreaterEq,  // >=
}

pub const EXPR_VOID : Expr = Expr::Constant(Const::Void);
pub const LEXPR_VOID : LExpr = 
    Located { data:EXPR_VOID, loc: ::location::EMPTY_SPAN };
