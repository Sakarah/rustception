use std::io::*;
use std::iter::Peekable;
use std::fmt;
use std::process::exit;
use std::io::{Error, ErrorKind};
use lexer;
use lexer::Token::*;
use lexer::LexerState::*;

use location::Location;

pub enum Token
{
    Integer(i32),
    Identifier(String),

    // "string"
    StringLitteral(String),

    // Keywords
    Else, False, Fn, If, Let, Mut, Return, Struct, True, While,

    // =, ., *, :, ;, ,, '
    Eq, Dot, Star, Colon, SemiColon, Comma, Apostrophe,

    // +, -, /, %, &&, ||
    Plus, Minus, Div, Mod, And, Or,

    // ==, !=, >=, <=
    DoubleEq, NotEq, Geq, Leq,

    // !, &
    Bang, Ampersand,

    // Brackets
    LeftParen, RightParen,      // ()
    LeftBrace, RightBrace,      // {}
    LeftBracket, RightBracket,  // []
    LeftChevron, RightChevron,  // <>
}

#[derive(PartialEq)]
enum LexerState
{
    ReadInt,
    ReadIdentifier,
    ReadQuotedString,
    InitialState,
}

impl LexerState
{
    fn clone(&self) -> LexerState {
        match *self  {
            ReadInt => ReadInt,
            ReadIdentifier => ReadIdentifier,
            ReadQuotedString => ReadQuotedString,
            InitialState => InitialState,
        }
    }
}

pub struct Lexer<R: Read>
{
    state: LexerState,
    value: String,
    input: Peekable<Bytes<R>>,
    start_loc: Location,
    end_loc: Location,
    what: String,                // error handling
    stop: bool
}

impl<R: Read> Lexer<R>
{
    /**
     * Create a new lexer parsing tokens from the given input.
     */
    pub fn from_channel(channel: R) -> Lexer<R>
    {
        Lexer {
            value: String::new(),
            input: channel.bytes().peekable(),
            state: lexer::LexerState::InitialState,
            start_loc: Location::new(0,1,1),
            end_loc: Location::new(0,1,1),
            what: String::new(),
            stop: false
        }
    }

    // Reduce the location to it's last character.
    fn reduce(&mut self)
    {
        self.start_loc.line = self.end_loc.line;
        self.start_loc.column = self.end_loc.column;
        self.start_loc.offset = self.end_loc.offset;
    }

    fn single_commentary(&mut self)
    {
        loop
        {
            match next_char(&mut self.input)
            {
                Some('\n') => { self.end_loc.extend('\n'); break },
                None => break,
                Some(c) => self.end_loc.extend(c)
            };
        }
    }

    fn nested_commentary(&mut self)
    {
        loop
        {
            match next_char(&mut self.input)
            {
                Some('/') => match peek_char(&mut self.input)
                {
                    Some('*') => {
                        self.input.next();
                        self.end_loc.extend('/');
                        self.end_loc.extend('*');
                        self.nested_commentary()
                    },
                    Some(_) => self.end_loc.extend('/'),
                    None => self.what = format!("Some commentary does not end")
                },
                Some('*') => match peek_char(&mut self.input)
                {
                    Some('/') => {
                        self.input.next();
                        self.end_loc.extend('*');
                        self.end_loc.extend('/');
                        break
                    },
                    Some(_) => self.end_loc.extend('*'),
                    None => self.what = format!("Some commentary does not end")
                },
                Some(c) => self.end_loc.extend(c),
                None => {
                    self.what = format!("Some commentary does not end");
                    self.stop = true;
                    break
                }
            };
        }
    }

    /**
     * Update the internal lexer state after reading one character. Optionally
     * produces a token.
     */
    fn consume(&mut self, c: char) -> Option<Token>
    {
        let mut token = None;

        // First match to decide whether the char should be taken or not
        match (self.state.clone(), c)
        {
            (ReadQuotedString, _) |
            (InitialState, _) |
            (ReadIdentifier, 'a' ... 'z') |
            (ReadIdentifier, 'A' ... 'Z') |
            (ReadIdentifier, '0' ... '9') |
            (ReadIdentifier, '_') |
            (ReadInt, '0' ... '9') |
            (ReadInt, '_') => { self.input.next(); self.end_loc.extend(c) },
            _ => ()
        }

        match (self.state.clone(), c)
        {
            // Case ReadInt
            (ReadInt, '0' ... '9') => self.value.push(c),
            (ReadInt, '_') => (),
            (ReadInt, _) => {
                match self.value.parse::<i32>() {
                    Ok(n) => {
                        self.value.clear();
                        token = Some(Integer(n))
                    },
                    Err(msg) => {
                        self.what = format!("Failed integer parsing:{}:{}:{}",
                                        self.start_loc.line,
                                        self.start_loc.column,
                                        msg);
                        self.stop = true
                    }
                };
                self.state = InitialState;
            },

            // Case ReadIdentifier
            (ReadIdentifier, 'a' ... 'z') |
            (ReadIdentifier, 'A' ... 'Z') |
            (ReadIdentifier, '0' ... '9') |
            (ReadIdentifier, '_') => self.value.push(c),
            (ReadIdentifier, _) => {
                match self.value.as_str()
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
                    _ => token = Some(Identifier(self.value.clone()))
                };
                self.value.clear();
                self.state = InitialState;
            },

            (ReadQuotedString, '"') => {
                token = Some(StringLitteral(self.value.clone()));
                self.value.clear();
                self.state = InitialState;
            },
            (ReadQuotedString, '\\') => match next_char(&mut self.input) {
                Some('n') => self.value.push('\n'),
                Some('t') => self.value.push('\t'),
                Some('\\') => self.value.push('\\'),
                Some('"') => self.value.push('\"'),
                Some('0') => self.value.push('\0'),
                Some(c) => {
                    self.what = format!("error: unknown escaped character: {} :{}:{}",
                                    c,
                                    self.end_loc.line,
                                    self.end_loc.column);
                    self.stop = true
                },
                None => {
                    self.what = format!("error: unfinished string litteral line {}",
                                    self.end_loc.line);
                    self.stop = true
                }
            },
            (ReadQuotedString, _) => self.value.push(c),

            // Case InitialState
            (InitialState, '0' ... '9') => {
                self.value.push(c);
                self.state = ReadInt
            },
            (InitialState, 'a' ... 'z') => {
                self.value.push(c);
                self.state = ReadIdentifier
            },
            (InitialState, '"') => self.state = ReadQuotedString,

            // Double characters token
            (InitialState, '&') => match peek_char(&mut self.input) {
                Some('&') => { self.input.next(); token = Some(And) },
                _ => token = Some(Ampersand)
            },
            (InitialState, '!') => match peek_char(&mut self.input) {
                Some('=') => { self.input.next(); token = Some(NotEq) },
                _ => token = Some(Bang)
            },
            (InitialState, '/') => match peek_char(&mut self.input) {
                Some('/') => self.single_commentary(),
                Some('*') => self.nested_commentary(),
                _ => token = Some(Div)
            },
            (InitialState, '<') => match peek_char(&mut self.input) {
                Some('=') => { self.input.next(); token = Some(Leq) },
                _ => token = Some(LeftChevron)
            },
            (InitialState, '>') => match peek_char(&mut self.input) {
                Some('=') => { self.input.next(); token = Some(Geq) },
                _ => token = Some(RightChevron)
            },
            (InitialState, '|') => match peek_char(&mut self.input) {
                Some('|') => { self.input.next(); token = Some(Or) },
                _ => {
                    self.what = format!("error: expected character '|' :{}:{}",
                                    self.end_loc.line,
                                    self.end_loc.column);
                    self.stop = true
                }
            },
            (InitialState, '=') => match peek_char(&mut self.input) {
                Some('=') => { self.input.next(); token = Some(DoubleEq) },
                _ => token = Some(Eq)
            },

            // Single character token
            (InitialState, '(') => token = Some(LeftParen),
            (InitialState, ')') => token = Some(RightParen),
            (InitialState, '[') => token = Some(LeftBracket),
            (InitialState, ']') => token = Some(RightBracket),
            (InitialState, '{') => token = Some(LeftBrace),
            (InitialState, '}') => token = Some(RightBrace),
            (InitialState, '.') => token = Some(Dot),
            (InitialState, '\'') => token = Some(Apostrophe),
            (InitialState, ',') => token = Some(Comma),
            (InitialState, ';') => token = Some(SemiColon),
            (InitialState, ':') => token = Some(Colon),
            (InitialState, '*') => token = Some(Star),
            (InitialState, '+') => token = Some(Plus),
            (InitialState, '-') => token = Some(Minus),
            (InitialState, '%') => token = Some(Mod),
            (InitialState, ' ') | (InitialState, '\n') |
            (InitialState, '\t') => self.reduce(),
            (InitialState, _) => {
                self.what = format!("error: illegal character {} :{}:{}",
                                    c,
                                    self.end_loc.line,
                                    self.end_loc.column - 1);
                self.stop = true
            }
        };
        token
    }
}

fn next_char<R: Read>(iter: &mut Peekable<Bytes<R>>) -> Option<char> {
    iter.next()
        .map( |x| match x {
            Ok(c) => c as char,
            Err(err) => { print!("{:?}", err); exit(42) }
        } )
}

fn peek_char<R: Read>(iter: &mut Peekable<Bytes<R>>) -> Option<char> {
    iter.peek()
        .map( |x| match x {
            &Ok(c) => c as char,
            &Err(ref err) => { print!("{:?}", err); exit(42) }
        } )
}

impl<R: Read> Iterator for Lexer<R>
{
    type Item = Result<(Location, Token, Location)>;

    fn next(&mut self) -> Option<Result<(Location, Token, Location)>>
    {
        // Reduce the location
        self.reduce();
        loop
        {
            if self.stop {
                let err = Error::new(ErrorKind::Other, self.what.clone());
                return Some(Err(err))
            };

            match peek_char(&mut self.input)
            {
                Some (c) => match self.consume(c) {
                    Some(token) =>
                        return Some(Ok((self.start_loc.clone(),
                                        token,
                                        self.end_loc.clone()))),
                    None => ()
                },
                None => break
            }
        }
        if self.state.clone() == ReadQuotedString
        {
            return Some(Err(Error::new(ErrorKind::Other,
                "Unfinished string literal")))
        }
        None
    }
}

impl fmt::Display for Token
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self
        {
            &Integer(n) => write!(f, "INT({})", n),
            &Identifier(ref s) => write!(f, "ID({})", s),
            &StringLitteral(ref s) => write!(f, "STR({})", s),

            &Else => write!(f, "ELSE"),
            &False => write!(f, "FALSE"),
            &Fn => write!(f, "FN"),
            &If => write!(f, "IF"),
            &Let => write!(f,"LET"),
            &Mut => write!(f, "MUT"),
            &Return => write!(f, "RETURN"),
            &Struct => write!(f, "STRUCT"),
            &True => write!(f, "TRUE"),
            &While => write!(f, "WHILE"),

            &Eq => write!(f,"="),
            &Dot => write!(f, "."),
            &Star => write!(f, "*"),
            &Colon => write!(f, ":"),
            &SemiColon => write!(f, ";"),
            &Comma => write!(f, ","),
            &Apostrophe => write!(f, "'"),

            &Plus => write!(f, "PLUS"),
            &Minus => write!(f, "MINUS"),
            &Div => write!(f, "DIV"),
            &Mod => write!(f, "MOD"),

            &And => write!(f, "AND"),
            &Or => write!(f, "OR"),

            &DoubleEq => write!(f, "EQ"),
            &NotEq => write!(f, "NEQ"),
            &Geq => write!(f, "GEQ"),
            &Leq => write!(f, "LEQ"),

            &Bang => write!(f, "BANG"),
            &Ampersand => write!(f, "AMPERSAND"),

            &LeftParen => write!(f, "("),
            &RightParen => write!(f, ")"),
            &LeftBrace => write!(f, "{{"),
            &RightBrace => write!(f, "}}"),
            &LeftBracket => write!(f, "["),
            &RightBracket => write!(f, "]"),
            &LeftChevron => write!(f, "<"),
            &RightChevron => write!(f, ">"),
        }
    }
}
