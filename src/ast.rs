type Ident = String;

struct Module // (<Fun> | <Struct>)*
{
    funs: Vec<Fun>,
    structs: Vec<Struct>
}

struct Fun // fn <Ident>(<Arg>,*) -> <Type> <Block>
{
    name: Ident,
    arguments: Vec<Arg>,
    returnType: Type,
    implementation: Block
}

struct Struct // struct <Ident> { (<Ident> : <Type>),* }
{
    name: Ident,
    fields: Vec<(Ident, Type)>
}

enum Type
{
    Void,                           // ()
    Basic(Ident),                   // <Ident>
    Parametrized(Ident,Vec<Type>),  // <Ident>'<'(<Type>),*'>'
    Ref(Box<Type>),                 // &<Type>
    MutRef(Box<Type>)               // &mut <Type>
}

enum Mut
{
    Constant,   // ε
    Mutable     // mut
}
struct Arg(Mut, Ident, Type); // <Mut> <Ident> : <Type>

struct Block(Vec<Instr>, Expr); // { <Instr>* <Expr>? }

enum Instr
{
    NoOp,                    // ;
    Expression(Expr),        // <Expr>;
    Let(Mut, Ident, Expr),   // let <Mut> <Ident> = <Expr>;
    While(Expr, Box<Block>), // while <Expr> <Block>
    Return(Expr),            // return <Expr>?;
    If(IfInstr)              // <IfInstr>
}

enum IfInstr
{
    SingleIf(Expr, Box<Block>),             // if <Expr> <Block>
    IfElse(Expr, Box<Block>, Box<Block>),   // if <Expr> <Block> else <Block>
    IfElseIf(Expr, Box<Block>, Box<IfInstr>)// if <Expr> <Block> else <IfInstr>
}

enum Expr
{
    Constant(Const),                        // <Const>
    Variable(Ident),                        // <Ident>
    BinaryOp(BinOp, Box<Expr>, Box<Expr>),  // <Expr1> <BinOp> <Expr2>
    UnaryOp(UnOp, Box<Expr>),               // <UnOp> <Expr>
    StructAccess(Box<Expr>, Ident),         // <Expr>.<Ident>
    MethodCall(Box<Expr>, Ident, Vec<Expr>),// <Expr>.<Ident>(<Expr>,*)
    ArrayAccess(Box<Expr>, Box<Expr>),      // <Expr>[<Expr>]
    FunctionCall(Ident, Vec<Expr>),         // <Ident>(<Expr>,*)
    StructConstr(Ident, Vec<(Ident, Expr)>),// <Ident>{ (<Ident>:<Expr>),* }
    VecConstr(Vec<Expr>),                   // vec![<Expr>,*]
    Print(String),                          // print!("<Chaîne>")
    NestedBlock(Box<Block>)                 // <Block>
}

enum Const
{
    Void,
    Int32(i32),
    Bool(bool)
}

enum BinOp
{
    Equal,      // ==
    NotEqual,   // !=
    Less,       // <
    LessEq,     // <=
    Greater,    // >
    GreaterEq,  // >=
    Add,        // +
    Sub,        // -
    Mul,        // *
    Div,        // /
    Mod,        // %
    And,        // &&
    Or,         // ||
    Assign      // =
}

enum UnOp
{
    Minus,      // -
    Not,        // !
    Deref,      // *
    Ref,        // &
    MutRef      // &mut
}

