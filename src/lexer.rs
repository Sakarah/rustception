use std;
use std::iter::Peekable;
use std::io;
use std::io::{Read,Bytes};
use std::fmt;
use lexer::Token::*;
use lexer::LexingError::*;
use lexer::LexerState::*;

use symbol::Symbol;
use location::{Location,Span};

//   _____     _
//  |_   _|__ | | _____ _ __
//    | |/ _ \| |/ / _ \ '_ \
//    | | (_) |   <  __/ | | |
//    |_|\___/|_|\_\___|_| |_|
//
pub enum Token
{
    Integer(i32),
    Identifier(Symbol),

    // "string"
    StringLiteral(Symbol),

    // Keywords
    Else, False, Fn, If, Let, Mut, Return, Struct, True, While,

    // =, ., *, :, ;, ,, '
    Eq, Dot, Star, Colon, SemiColon, Comma, Apostrophe,

    // +, -, /, %, &&, ||
    Plus, Minus, Slash, Percent, And, Or,

    // ==, !=, >=, <=
    DoubleEq, NotEq, Geq, Leq,

    // !, &
    Bang, Ampersand,

    // Brackets
    LeftParen, RightParen,      // ()
    LeftBrace, RightBrace,      // {}
    LeftBracket, RightBracket,  // []
    LeftChevron, RightChevron,  // <>

    RightArrow                  // ->
}

impl fmt::Display for Token
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match *self
        {
            Integer(n) => write!(f, "Int({})", n),
            Identifier(ref s) => write!(f, "Ident({})", s),
            StringLiteral(ref s) => write!(f, "Str({})", s),

            Else => write!(f, "else"),
            False => write!(f, "false"),
            Fn => write!(f, "fn"),
            If => write!(f, "if"),
            Let => write!(f,"let"),
            Mut => write!(f, "mut"),
            Return => write!(f, "return"),
            Struct => write!(f, "struct"),
            True => write!(f, "true"),
            While => write!(f, "while"),

            Eq => write!(f,"="),
            Dot => write!(f, "."),
            Star => write!(f, "*"),
            Colon => write!(f, ":"),
            SemiColon => write!(f, ";"),
            Comma => write!(f, ","),
            Apostrophe => write!(f, "'"),

            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Slash => write!(f, "/"),
            Percent => write!(f, "%"),

            And => write!(f, "&&"),
            Or => write!(f, "||"),

            DoubleEq => write!(f, "=="),
            NotEq => write!(f, "!="),
            Geq => write!(f, ">"),
            Leq => write!(f, "<"),

            Bang => write!(f, "!"),
            Ampersand => write!(f, "&"),

            LeftParen => write!(f, "("),
            RightParen => write!(f, ")"),
            LeftBrace => write!(f, "{{"),
            RightBrace => write!(f, "}}"),
            LeftBracket => write!(f, "["),
            RightBracket => write!(f, "]"),
            LeftChevron => write!(f, "<"),
            RightChevron => write!(f, ">"),

            RightArrow => write!(f, "->")
        }
    }
}

//   _              _             _____
//  | |    _____  _(_)_ __   __ _| ____|_ __ _ __ ___  _ __
//  | |   / _ \ \/ / | '_ \ / _` |  _| | '__| '__/ _ \| '__|
//  | |__|  __/>  <| | | | | (_| | |___| |  | | | (_) | |
//  |_____\___/_/\_\_|_| |_|\__, |_____|_|  |_|  \___/|_|
//                          |___/
pub enum LexingError
{
    IoError(io::Error),
    UnfinishedComment,
    UnfinishedStringLitteral,
    Utf8Error(std::string::FromUtf8Error),
    UnknownEscapedChar(u8, Location),
    ExpectedCharacter(char, Location),
    IllegalCharacter(u8, Location)
}

impl From<io::Error> for LexingError
{
    fn from(err: io::Error) -> Self { IoError(err) }
}

impl From<std::string::FromUtf8Error> for LexingError
{
    fn from(err: std::string::FromUtf8Error) -> Self { Utf8Error(err) }
}

type Result<T> = std::result::Result<T,LexingError>;

//   _                       ____  _        _
//  | |    _____  _____ _ __/ ___|| |_ __ _| |_ ___
//  | |   / _ \ \/ / _ \ '__\___ \| __/ _` | __/ _ \
//  | |__|  __/>  <  __/ |   ___) | || (_| | ||  __/
//  |_____\___/_/\_\___|_|  |____/ \__\__,_|\__\___|
//
#[derive(PartialEq, Clone, Copy)]
enum LexerState
{
    ReadInt,
    ReadIdentifier,
    ReadQuotedString,
    InitialState,
}

//   _
//  | |    _____  _____ _ __
//  | |   / _ \ \/ / _ \ '__|
//  | |__|  __/>  <  __/ |
//  |_____\___/_/\_\___|_|
//
pub struct Lexer<R:Read>
{
    state: LexerState,
    value: Vec<u8>,
    input: Peekable<Bytes<R>>,
    loc: Span
}

impl<R:Read> Lexer<R>
{
    /**
     * Create a new lexer parsing tokens from the given input.
     */
    pub fn from_channel(channel: R, filename: String) -> Lexer<R>
    {
        Lexer {
            value: Vec::new(),
            input: channel.bytes().peekable(),
            state: LexerState::InitialState,
            loc: Span::new(Symbol::from(filename))
        }
    }

    fn single_commentary(&mut self) -> Result<()>
    {
        loop
        {
            match self.input.next()
            {
                None => break,
                Some(c) => {
                    let c = c?;
                    self.loc.extend(c);
                    if c == b'\n' { break }
                }
            };
        }
        Ok(())
    }

    fn nested_commentary(&mut self) -> Result<()>
    {
        loop
        {
            let c = self.input.next().ok_or(UnfinishedComment)??;
            self.loc.extend(c);
            match c
            {
                b'/' =>
                {
                    match self.input.peek()
                    {
                        None => return Err(UnfinishedComment),
                        Some(&Err(_)) =>
                            return Err(IoError(self.input.next().unwrap()
                                               .unwrap_err())),
                        Some(&Ok(b'*')) =>
                        {
                            self.input.next();
                            self.loc.extend(b'*');
                            self.nested_commentary()?
                        }
                        _ => ()
                    }
                },
                b'*' =>
                {
                    match self.input.peek()
                    {
                        None => return Err(UnfinishedComment),
                        Some(&Err(_)) =>
                            return Err(IoError(self.input.next().unwrap()
                                               .unwrap_err())),
                        Some(&Ok(b'/')) =>
                        {
                            self.input.next();
                            self.loc.extend(b'/');
                            break
                        }
                        _ => ()
                    }
                }
                _ => ()
            };
        }
        Ok(())
    }

    /**
     * Update the internal lexer state after reading one character. Optionally
     * produces a token.
     */
    fn consume(&mut self, c: u8) -> Result<Option<Token>>
    {
        let mut token = None;

        // First match to decide whether the char should be taken or not
        match (self.state, c)
        {
            (ReadQuotedString, _) |
            (InitialState, _) |
            (ReadIdentifier, b'a' ... b'z') |
            (ReadIdentifier, b'A' ... b'Z') |
            (ReadIdentifier, b'0' ... b'9') |
            (ReadIdentifier, b'_') |
            (ReadInt, b'0' ... b'9') |
            (ReadInt, b'_') => { self.input.next(); self.loc.extend(c) },
            _ => ()
        }

        match (self.state, c)
        {
            // Case ReadInt
            (ReadInt, b'0' ... b'9') => self.value.push(c),
            (ReadInt, b'_') => (),
            (ReadInt, _) =>
            {
                token = Some(Integer(i32_from_digits(&self.value)));
                self.value = Vec::new();
                self.state = InitialState;
            },

            // Case ReadIdentifier
            (ReadIdentifier, b'a' ... b'z') |
            (ReadIdentifier, b'A' ... b'Z') |
            (ReadIdentifier, b'0' ... b'9') |
            (ReadIdentifier, b'_') => self.value.push(c),
            (ReadIdentifier, _) =>
            {
                let raw_ident = self.value.drain(..).collect();
                let identifier = String::from_utf8(raw_ident)?;
                match identifier.as_ref()
                {
                    "else" => token = Some(Else),
                    "false" => token = Some(False),
                    "fn" => token = Some(Fn),
                    "if" => token = Some(If),
                    "let" => token = Some(Let),
                    "mut" => token = Some(Mut),
                    "return" => token = Some(Return),
                    "struct" => token = Some(Struct),
                    "true" => token = Some(True),
                    "while" => token = Some(While),
                    _ => token = Some(Identifier(Symbol::from(identifier)))
                };
                self.state = InitialState;
            },

            (ReadQuotedString, b'"') =>
            {
                let raw_str = self.value.drain(..).collect();
                let string = String::from_utf8(raw_str)?;
                token = Some(StringLiteral(Symbol::from(string)));
                self.state = InitialState;
            },
            (ReadQuotedString, b'\\') =>
            {
                match self.input.next().ok_or(UnfinishedStringLitteral)??
                {
                    b'n' => self.value.push(b'\n'),
                    b't' => self.value.push(b'\t'),
                    b'\\' => self.value.push(b'\\'),
                    b'"' => self.value.push(b'\"'),
                    b'0' => self.value.push(b'\0'),
                    c => return Err(UnknownEscapedChar(c, self.loc.end))
                }
            },
            (ReadQuotedString, _) => self.value.push(c),

            // Case InitialState
            (InitialState, b'0'...b'9') =>
            {
                self.value.push(c);
                self.state = ReadInt
            },
            (InitialState, b'a'...b'z') | (InitialState, b'A'...b'Z') =>
            {
                self.value.push(c);
                self.state = ReadIdentifier
            },
            (InitialState, b'"') => self.state = ReadQuotedString,

            // Double characters token
            (InitialState, b'&') => match self.input.peek() {
                Some(&Ok(b'&')) => { self.input.next(); token = Some(And) },
                _ => token = Some(Ampersand)
            },
            (InitialState, b'!') => match self.input.peek() {
                Some(&Ok(b'=')) => { self.input.next(); token = Some(NotEq) },
                _ => token = Some(Bang)
            },
            (InitialState, b'/') => match self.input.peek() {
                Some(&Ok(b'/')) => self.single_commentary()?,
                Some(&Ok(b'*')) => self.nested_commentary()?,
                _ => token = Some(Slash)
            },
            (InitialState, b'<') => match self.input.peek() {
                Some(&Ok(b'=')) => { self.input.next(); token = Some(Leq) },
                _ => token = Some(LeftChevron)
            },
            (InitialState, b'>') => match self.input.peek() {
                Some(&Ok(b'=')) => { self.input.next(); token = Some(Geq) },
                _ => token = Some(RightChevron)
            },
            (InitialState, b'|') => match self.input.peek() {
                Some(&Ok(b'|')) => { self.input.next(); token = Some(Or) },
                _ => return Err(ExpectedCharacter('|', self.loc.end))
            },
            (InitialState, b'=') => match self.input.peek() {
                Some(&Ok(b'=')) =>
                    { self.input.next(); token = Some(DoubleEq) },
                _ => token = Some(Eq)
            },
            (InitialState, b'-') => match self.input.peek() {
                Some(&Ok(b'>')) =>
                    { self.input.next(); token = Some(RightArrow) },
                _ => token = Some(Minus)
            }

            // Single character token
            (InitialState, b'(') => token = Some(LeftParen),
            (InitialState, b')') => token = Some(RightParen),
            (InitialState, b'[') => token = Some(LeftBracket),
            (InitialState, b']') => token = Some(RightBracket),
            (InitialState, b'{') => token = Some(LeftBrace),
            (InitialState, b'}') => token = Some(RightBrace),
            (InitialState, b'.') => token = Some(Dot),
            (InitialState, b'\'') => token = Some(Apostrophe),
            (InitialState, b',') => token = Some(Comma),
            (InitialState, b';') => token = Some(SemiColon),
            (InitialState, b':') => token = Some(Colon),
            (InitialState, b'*') => token = Some(Star),
            (InitialState, b'+') => token = Some(Plus),
            (InitialState, b'%') => token = Some(Percent),
            (InitialState, b' ') | (InitialState, b'\n') |
            (InitialState, b'\t') => self.loc.reduce(),
            (InitialState, _) => return Err(IllegalCharacter(c, self.loc.end))
        };
        Ok(token)
    }
}

use std::i32;
/**
 * Transform an vector of digits (as utf8 chars) to an i32 integer
 */
fn i32_from_digits(digits:&Vec<u8>) -> i32
{

    let mut acc = 0i32;
    for d in digits
    {
        acc = acc.wrapping_mul(10);
        acc = acc.wrapping_add((d - b'0') as i32);
    }
    return acc
}

impl<R:Read> Iterator for Lexer<R>
{
    type Item = Result<(Location, Token, Location)>;

    fn next(&mut self) -> Option<Self::Item>
    {
        // Reduce the location
        self.loc.reduce();
        loop
        {
            match self.input.peek()
            {
                Some(&Ok(c)) => match self.consume(c)
                {
                    Ok(Some(token)) =>
                        return Some(Ok((self.loc.start.clone(),
                                        token,
                                        self.loc.end.clone()))),
                    Ok(None) => (),
                    Err(e) => return Some(Err(e))
                },
                Some(&Err(_)) =>
                    return Some(Err(IoError(self.input.next()
                                            .unwrap().unwrap_err()))),
                None => break
            }
        }
        if *&self.state == ReadQuotedString
        {
            return Some(Err(UnfinishedStringLitteral))
        }
        None
    }
}

