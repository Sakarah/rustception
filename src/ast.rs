pub type Ident = String;

pub struct Module // (<Fun> | <Struct>)*
{
    pub funs: Vec<Fun>,
    pub structs: Vec<Struct>
}

pub struct Fun // fn <Ident>(<Arg>,*) -> <Type> <Block>
{
    pub name: Ident,
    pub arguments: Vec<Arg>,
    pub return_type: Type,
    pub body: Block
}

pub struct Struct // struct <Ident> { (<Ident> : <Type>),* }
{
    pub name: Ident,
    pub fields: Vec<(Ident, Type)>
}

pub enum Decl // Used internally by parser
{
    Function(Fun),
    Structure(Struct)
}

pub enum Type
{
    Void,                           // ()
    Basic(Ident),                   // <Ident>
    Parametrized(Ident,Vec<Type>),  // <Ident>'<'(<Type>),*'>'
    Ref(Box<Type>),                 // &<Type>
    MutRef(Box<Type>)               // &mut <Type>
}

pub enum Mut
{
    Constant,   // ε
    Mutable     // mut
}
pub struct Arg(pub Mut, pub Ident, pub Type); // <Mut> <Ident> : <Type>

pub struct Block(pub Vec<Instr>, pub Expr); // { <Instr>* <Expr>? }

pub enum Instr
{
    NoOp,                    // ;
    Expression(Expr),        // <Expr>;
    Let(Mut, Ident, Expr),   // let <Mut> <Ident> = <Expr>;
    While(Expr, Box<Block>), // while <Expr> <Block>
    Return(Expr),            // return <Expr>?;
    If(IfInstr)              // <IfInstr>
}

pub enum IfInstr
{
    Single(Expr, Box<Block>, Box<Block>), // if <Expr> <Block> (else <Block>)?
    Nested(Expr, Box<Block>, Box<IfInstr>)// if <Expr> <Block> else <IfInstr>
}

pub enum Expr
{
    Assignment(Box<Expr>, Box<Expr>),       // <Expr> = <Expr>
    Or(Box<Expr>, Box<Expr>),               // <Expr> || <Expr>
    And(Box<Expr>, Box<Expr>),              // <Expr> && <Expr>
    Comparison(Comp, Box<Expr>, Box<Expr>), // <Expr> <Comp> <Expr>

    Addition(Box<Expr>, Box<Expr>),         // <Expr> + <Expr>
    Substraction(Box<Expr>, Box<Expr>),     // <Expr> - <Expr>
    Multiplication(Box<Expr>, Box<Expr>),   // <Expr> * <Expr>
    Division(Box<Expr>, Box<Expr>),         // <Expr> / <Expr>
    Remainder(Box<Expr>, Box<Expr>),        // <Expr> % <Expr>

    Minus(Box<Expr>),                       // - <Expr>
    Not(Box<Expr>),                         // ! <Expr>
    Deref(Box<Expr>),                       // * <Expr>
    Ref(Box<Expr>),                         // & <Expr>
    MutRef(Box<Expr>),                      // & mut <Expr>

    ArrayAccess(Box<Expr>, Box<Expr>),      // <Expr>[<Expr>]
    Attribute(Box<Expr>, Ident),            // <Expr>.<Ident>
    MethodCall(Box<Expr>, Ident, Vec<Expr>),// <Expr>.<Ident>(<Expr>,*)

    Constant(Const),                        // <Const>
    Variable(Ident),                        // <Ident>
    FunctionCall(Ident, Vec<Expr>),         // <Ident>(<Expr>,*)
    StructConstr(Ident, Vec<(Ident, Expr)>),// <Ident>{ (<Ident>:<Expr>),* }
    ListMacro(Ident,Vec<Expr>),             // <Ident>![<Expr>,*]
    StringMacro(Ident,String),              // <Ident>!("<Chaîne>")
    NestedBlock(Box<Block>)                 // <Block>
}

pub enum Const
{
    Void,
    Int32(i32),
    Bool(bool)
}

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
