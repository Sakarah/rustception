use std;
use std::iter::Peekable;
use std::fmt;
use std::cmp;
use lexer;
use lexer::Token::*;
use lexer::LexerState::*;

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
    Plus, Sub, Div, Mod, And, Or, 
    
    // ==, !=, >=, <=
    DoubleEq, NotEq, Geq, Leq,

    // !, &
    Not, Ref,

    // Brackets
    LeftParen, RightParen,      // ()
    LeftBrace, RightBrace,      // {}
    LeftBracket, RightBracket,  // []
    LeftChevron, RightChevron,  // <>
}

enum LexerState
{
    ReadInt,
    ReadIdentifier,
    ReadQuotedString,
    InitialState,
}

pub struct Lexer<'a>
{
    state: LexerState,
    value: String,
    input: Peekable<std::str::Chars<'a>>,
    reinjection: Option<char>
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

impl<'a> Lexer<'a>
{
    /**
     * Create a new lexer parsing tokens from the given input.
     */
    pub fn new<'b>(input: std::str::Chars<'b>) -> Lexer<'b>
    {
        Lexer {
            value: String::new(),
            input: input.peekable(),
            state: lexer::LexerState::InitialState,
            reinjection: None
        }
    }

    fn single_commentary(&mut self)
    {
        loop
        {
            match self.input.next()
            {
                Some('\n') | None => break,
                _ => ()
            };
        }
    }

    fn nested_commentary(&mut self)
    {
        loop 
        {
            match self.input.next()
            {
                Some('/') => match self.input.peek() 
                {
                    Some(&'*') => {self.input.next(); self.nested_commentary()},
                    Some(_) => (),
                    None => panic!("Some commentary does not end")
                },
                Some('*') => match self.input.peek()
                {
                    Some(&'/') => { self.input.next(); break }
                    Some(_) => (),
                    None => panic!("Some commentary does not end")
                },
                Some(_) => (),
                None => panic!("Some commentary does not end")
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
        match (self.state.clone(), c)
        {
            // Case ReadInt
            (ReadInt, '0' ... '9') => self.value.push(c),
            (ReadInt, _) => {
                match self.value.parse::<i32>() {
                    Ok(n) => {
                        self.value.clear();
                        token = Some(Integer(n))
                    },
                    Err(msg) => panic!("failed integer parsing: {}", msg)
                };
                self.state = InitialState;
                self.reinjection = Some(c)
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
                self.reinjection = Some(c)
            },

            (ReadQuotedString, '"') => {
                token = Some(StringLitteral(self.value.clone()));
                self.value.clear();
                self.state = InitialState;
            }
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
            (InitialState, '&') => match self.input.peek() {
                Some(&'&') => { self.input.next(); token = Some(And) },
                _ => token = Some(Ref) 
            },
            (InitialState, '!') => match self.input.peek() {
                Some(&'=') => { self.input.next(); token = Some(NotEq) },
                _ => token = Some(Not)
            },
            (InitialState, '/') => match self.input.peek() {
                Some(&'/') => self.single_commentary(),
                Some(&'*') => self.nested_commentary(),
                _ => token = Some(Div)
            },
            (InitialState, '<') => match self.input.peek() {
                Some(&'=') => { self.input.next(); token = Some(Leq) },
                _ => token = Some(LeftChevron)
            },
            (InitialState, '>') => match self.input.peek() {
                Some(&'=') => { self.input.next(); token = Some(Geq) },
                _ => token = Some(RightChevron)
            },
            (InitialState, '|') => match self.input.peek() {
                Some(&'|') => { self.input.next(); token = Some(Or) },
                _ => panic!("Expected character |")
            },
            (InitialState, '=') => match self.input.peek() {
                Some(&'=') => { self.input.next(); token = Some(DoubleEq) },
                _ => token = Some(Eq)
            },

            // Simple character token
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
            (InitialState, '-') => token = Some(Sub),
            (InitialState, '%') => token = Some(Mod),
            (InitialState, ',') => token = Some(Comma),
            (InitialState, ' ') | (InitialState, '\n') => (),
            (InitialState, _) => panic!("Unexpected character {}", c)
        };
        token
    }
}

impl<'a> Iterator for Lexer<'a>
{
    type Item = Token;

    fn next(&mut self) -> Option<Token>
    {
        loop
        {
            match self.reinjection {
                Some (c) => {
                    match self.consume(c) {
                        Some(token) => { 
                            self.reinjection = None;
                            return Some(token)
                        },
                        None => self.reinjection = None
                    };
                      
                },
                None => match self.input.next() {
                    Some (c) =>
                    match self.consume(c) {
                        Some(token) => return Some(token),
                        None => ()
                    },
                    None => break
                }
            }
        }
        None
    }
}

impl cmp::PartialEq for Token
{
    fn eq(&self, other: &Token) -> bool
    {
        match (self, other)
        {
            (&Integer(_), &Integer(_)) |
            (&Identifier(_), &Identifier(_)) |
            (&StringLitteral(_), &StringLitteral(_)) |

            (&Else, &Else) | (&False, &False) | (&Fn, &Fn) | (&If, &If) |
            (&Let, &Let) | (&Mut, &Mut) | (&Return, &Return) | 
            (&Struct, &Struct) | (&True, &True) | (&While, &While) |

            (&Eq, &Eq) | (&Dot, &Dot) | (&Star, &Star) | (&Colon, &Colon) |
            (&SemiColon, &SemiColon) | (&Comma, &Comma) |
            (&Apostrophe, &Apostrophe) |

            (&Plus, &Plus) | (&Sub, &Sub) | (&Div, &Div) | (&Mod, &Mod) |
            (&And, &And) | (&Or, &Or) |

            (&DoubleEq, &DoubleEq) | (&NotEq, &NotEq) | 
            (&Geq, &Geq) | (&Leq, &Leq) |

            (&Not, &Not) | (&Ref, &Ref) | 
            (&LeftParen, &LeftParen) | (&RightParen, &RightParen) |
            (&LeftBrace, &LeftBrace) | (&RightBrace, &RightBrace) |
            (&LeftChevron, &LeftChevron) | (&RightChevron, &RightChevron) |
            (&LeftBracket, &LeftBracket) | (&RightBracket, &RightBracket)
                => true,
            _   => false
        }
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
            &Else => write!(f, "ELSE"),
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
            &Sub => write!(f, "MINUS"),
            &Div => write!(f, "DIV"),
            &Mod => write!(f, "MOD"),

            &And => write!(f, "AND"),
            &Or => write!(f, "OR"),

            &DoubleEq => write!(f, "EQ"),
            &NotEq => write!(f, "NEQ"),
            &Geq => write!(f, "GEQ"),
            &Leq => write!(f, "LEQ"),

            &Not => write!(f, "NOT"),
            &Ref => write!(f, "REF"),

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