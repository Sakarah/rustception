use std::io;
use std::path;
use std::fmt;
use lexer::{LexingError,Token};
use lalrpop_util::ParseError;
use typing::TypingError;
use borrow_checker::BorrowError;
use location::{Location,Span,Located};

pub enum Error
{
    NoInputFile,
    OpenFileError(path::PathBuf, io::Error),
    WriteFileError(io::Error),
    ParsingError(ParseError<Location, Token, LexingError>),
    TypingError(Located<TypingError>),
    BorrowCheckingError(Located<BorrowError>),
}

impl fmt::Display for Error
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match *self
        {
            Error::NoInputFile =>
                write!(f, "You must give a filename as argument"),
            Error::OpenFileError(ref filename, ref err) =>
                write!(f, "Unable to open file \"{}\": {}",
                       filename.display(), err),
            Error::WriteFileError(ref err) =>
                write!(f, "Unable to write output: {}", err),
            Error::ParsingError(ref err) =>
            {
                match *err
                {
                    ParseError::InvalidToken { ref location } =>
                        write!(f, "{}:\n\
                               Parsing error: Invalid token", location),
                    ParseError::UnrecognizedToken { ref token, ref expected } =>
                    {
                        if let Some((start, ref tok, end)) = *token
                        {
                            write!(f, "{}:\n\
                                   Parsing error: Unrecognized token `{}`",
                                   Span{start, end}, tok)?;
                        }
                        else
                        {
                            write!(f, "Parsing error: Unexpected end of file")?;
                        }
                        if !expected.is_empty()
                        {
                            for (i, e) in expected.iter().enumerate()
                            {
                                let sep = match i
                                {
                                    0 => "\nExpected one of",
                                    _ if i < expected.len() - 1 => ",",
                                    _ => " or"
                                };
                                write!(f, "{} {}", sep, e)?;
                            }
                            write!(f, "")?;
                        }
                        Ok(())
                    }
                    ParseError::ExtraToken { token: (start, ref tok, end) } =>
                        write!(f, "{}:\nParsing error: Extra token `{}`",
                               Span{start, end}, tok),
                    ParseError::User { ref error } =>
                    {
                        match *error
                        {
                            LexingError::IoError(ref err) =>
                                write!(f, "IO error: {}", err),
                            LexingError::UnfinishedComment =>
                                write!(f, "Lexing error: \
                                       Some commentary does not end"),
                            LexingError::UnfinishedStringLitteral =>
                                write!(f, "Lexing error: \
                                       Unfinished string litteral"),
                            LexingError::Utf8Error(ref err) =>
                                write!(f, "UTF8 conversion error: {}", err),
                            LexingError::UnknownEscapedChar(c, pos) =>
                                write!(f, "{}:\nLexing error: \
                                       Unknown escaped character {}",
                                       pos, c as char),
                            LexingError::ExpectedCharacter(exp, pos) =>
                                write!(f, "{}:\nLexing error: \
                                       Expected character '{}'",
                                       pos, exp),
                            LexingError::IllegalCharacter(ch, pos) =>
                                write!(f, "{}:\nLexing error: \
                                       Illegal character '{}'",
                                       pos, ch as char)
                        }
                    }
                }
            }
            Error::TypingError(ref err) =>
            {
                write!(f, "{}:\nTyping error: ", err.loc)?;
                match err.data
                {
                    TypingError::MultipleFuncDecl(fun_name) =>
                        write!(f, "Function `{}` was already declared before",
                               fun_name),
                    TypingError::MultipleStructDecl(struc_name) =>
                        write!(f, "Structure `{}` was already declared before",
                               struc_name),
                    TypingError::MultipleFieldDecl(field_name) =>
                        write!(f, "Field `{}` was already declared before",
                               field_name),
                    TypingError::UnknownType(type_name) =>
                        write!(f, "Unknown type `{}`", type_name),
                    TypingError::UnknownParametrizedType(type_name) =>
                        write!(f, "Unknown parametrized type `{}`", type_name),
                    TypingError::NotParametrizedType(type_name) =>
                        write!(f, "Type `{}` is not parametrized but is used \
                               with a template parameter", type_name),
                    TypingError::MismatchedTypes { ref found, ref expected } =>
                        write!(f, "Mismatched types.\n\
                               Expected `{}` and found `{}`", expected, found),
                    TypingError::FunctionReturnBorrowed(ref typ) =>
                        write!(f, "A function cannot return the borrowed type \
                               `{}`\nThis is a Petit-Rust limitation.", typ),
                    TypingError::MultipleArgumentDecl(arg_name) =>
                        write!(f, "The identifier `{}` is bound more than once\
                               in this parameter list", arg_name),
                    TypingError::AssignmentOnRvalue =>
                        write!(f, "Cannot perform assignment on rvalue"),
                    TypingError::AssignmentOnConstant =>
                        write!(f, "Cannot perform assignment on constant"),
                    TypingError::CannotDeref(ref typ) =>
                        write!(f, "Cannot dereference value of the not \
                               borrowed type `{}`", typ),
                    TypingError::BorrowOnRvalue =>
                        write!(f, "Cannot borrow an rvalue"),
                    TypingError::MutBorrowOnConstant =>
                        write!(f, "Cannot take a mutable borrow on constant"),
                    TypingError::VariableUnbound(ident) =>
                        write!(f, "Variable identifier `{}` is unbound in this \
                               context", ident),
                    TypingError::UnknownFunction(fun_name) =>
                        write!(f, "Function `{}` is not declared", fun_name),
                    TypingError::WrongNumberOfArguments { found, expected } =>
                        write!(f, "This function takes {} arguments but {} \
                               were supplied", expected, found),
                    TypingError::FunctionCallOnVariable(id) =>
                        write!(f, "`{}` is not a function so it cannot be \
                               called", id),
                    TypingError::UnknownMacro(macro_name) =>
                        write!(f, "Unknown macro `{}`", macro_name),
                    TypingError::CyclicStruct(struct_name) =>
                        write!(f, "Struct `{}` is circular (i.e. it would have \
                               an infinite size)\nConsider using `Vec<{}>` to \
                               break cycles by adding indirection",
                               struct_name, struct_name),
                    TypingError::BorrowedInsideStruct(field_name, ref typ) =>
                        write!(f, "Field `{}` has the borrowed type `{}`.\n\
                               Borrowed types are forbidden inside structs.\n\
                               This is a Petit-Rust limitation.",
                               field_name, typ),
                    TypingError::InvalidFieldName { field, struc } =>
                        write!(f, "Invalid field name `{}` for struct `{}`",
                               field, struc),
                    TypingError::MultipleFieldInit(field_name) =>
                        write!(f, "Field `{}` is initialized more than once",
                               field_name),
                    TypingError::LackingField { field, struc } =>
                        write!(f, "Lacking field `{}` for struct `{}`",
                               field, struc),
                    TypingError::FieldAccessOnNonStruct(ref typ) =>
                        write!(f, "Field access on non struct type `{}`", typ),
                    TypingError::UnknownStruct(struct_name) =>
                        write!(f, "Unknown struct `{}`", struct_name),
                    TypingError::ArrayAccessOnRvalue =>
                        write!(f, "Cannot perform an array access on rvalue"),
                    TypingError::ArrayAccessOnScalarType(ref typ) =>
                        write!(f, "Cannot perform an array access on a value \
                               of type `{}`", typ),
                    TypingError::UnknownMethod(method_name, ref typ) =>
                        write!(f, "Unknown method `{}` for type `{}`",
                               method_name, typ),
                }
            }
            Error::BorrowCheckingError(ref err) =>
            {
                write!(f, "{}:\nBorrow checking error: ", err.loc)?;
                match err.data
                {
                    BorrowError::LifetimeTooShort(id) =>
                        write!(f, "`{}` has a too short lifetime to be \
                               borrowed here", id),
                    BorrowError::VariableOutliveValue(id) =>
                        write!(f, "`{}` outlives its borrowed value", id),
                    BorrowError::BorrowAfterMove(id) =>
                        write!(f, "Cannot borrow the moved variable `{}`", id),
                    BorrowError::BorrowAfterMutBorrow(id) =>
                        write!(f, "Cannot borrow the variable `{}` that was \
                               already borrowed mutably", id),
                    BorrowError::MutBorrowAfterBorrow(id) =>
                        write!(f, "Cannot borrow mutably the variable `{}` \
                               that was already borrowed", id),
                    BorrowError::UsedAfterMove(id) =>
                        write!(f, "`{}` is used here after beeing moved", id),
                    BorrowError::MovedAfterBorrow(id) =>
                        write!(f, "Cannot move the borrowed value `{}`", id),
                    BorrowError::ReassignAfterBorrow(id) =>
                        write!(f, "Cannot reassign to the borrowed value `{}`",
                               id),
                    BorrowError::MoveOutOfDerefValue =>
                        write!(f, "Cannot move out of dereferenced value"),
                    BorrowError::MoveOutOfIndexed =>
                        write!(f, "Cannot move out of indexed content"),
                    BorrowError::MismatchedLifetimes =>
                        write!(f, "Mismatched lifetimes"),
                    BorrowError::UnresolvedType =>
                        write!(f, "Unresolved type found"),
                    BorrowError::UnresolvedLifetime =>
                        write!(f, "Cannot resolve lifetime inside nested
                               references here.\n\
                               Nested references are forbidden inside `vec!` and
                               `if` expression results.\n\
                               This is a prustc limitation."),
                }
            }
        }
    }
}

