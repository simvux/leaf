use logos::*;
use lumina_util::Tr;
use std::fmt;

#[derive(PartialEq, Debug, Clone, Logos)]
#[logos(subpattern ident = "[_a-zA-Z][-_0-9a-zA-Z:]*")]
pub enum Token {
    #[regex("\\d+", |lex| lex.slice().parse())]
    Int(i64),
    #[regex("\\-?\\d+\\.\\d+", |lex| lex.slice().parse())]
    Float(f64),

    #[regex("\\n+")]
    NewLines,

    #[regex(" +")]
    Spaces,

    #[regex("//[^\n]*")]
    LineComment,

    #[regex("\"(?:[^\"]|\")*")]
    StringLiteral,

    #[regex("(?&ident)")]
    Identifier,

    #[regex("[+|-|/|*|%|@|$|?|^|~|<|>|=]*")]
    Operator,

    // #[regex("/\\*([^*]|\\*+[^*/])*\\*+/")]
    // MultiLineComment,
    #[token("=")]
    Equals,
    #[token("where")]
    Where,
    #[token("when")]
    When,
    #[token("match")]
    Match,
    #[token("with")]
    With,
    #[token("for")]
    For,
    #[token("as")]
    As,
    #[token(".")]
    Dot,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
    #[token("todo")]
    Todo,
    #[token("(")]
    OpenParen,
    #[token(")")]
    CloseParen,
    #[token("{")]
    OpenCurly,
    #[token("[")]
    OpenList,
    #[token("]")]
    CloseList,
    #[token("}")]
    CloseCurly,
    #[token("attributes")]
    Attributes,
    #[token("true")]
    BoolTrue,
    #[token("false")]
    BoolFalse,
    #[token("self")]
    TypeSelfStrict,
    #[token(",")]
    Comma,
    #[token("->")]
    Arrow,
    #[token("=>")]
    FatArrow,
    #[token("next")]
    Next,
    #[regex("<<|<\\||\\$|<:")]
    PipeLeft,
    #[regex(">>|\\|>|:>")]
    PipeRight,
    #[token("let")]
    Let,
    #[token("in")]
    In,
    #[token("mut")]
    Mut,
    #[token("debug")]
    Debug,
    #[token("::")]
    DoubleColon,
    #[token("first")]
    First,
    #[token("\\")]
    Lambda,
    #[token("#")]
    Pass,
    #[token("_")]
    Underscore,
    #[token("fn")]
    HeaderFn,
    #[token("struct")]
    HeaderStruct,
    #[token("enum")]
    HeaderEnum,
    #[token("trait")]
    HeaderTrait,
    #[token("use")]
    HeaderUse,
    #[token("impl")]
    HeaderImpl,

    #[error]
    Error,

    EOF,
}

impl Token {
    pub fn track(self, span: std::ops::Range<usize>) -> Tr<Self> {
        Tr::tr(span, self)
    }

    pub fn describe(&self) -> &'static str {
        use Token::*;

        match self {
            Int(_) => "integer",
            Float(_) => "float",
            NewLines => "new line",
            Spaces => "space",
            LineComment => "single line comment",
            StringLiteral => "string literal",
            Identifier => "identifier",
            Operator => "operator",
            Equals => "equals",
            Where => "where binding",
            When => "when",
            Match => "match expression",
            With => "with",
            For => "for",
            As => "cast",
            Dot => "dot",
            If => "if expression",
            Then => "then",
            Else => "else",
            Todo => "todo marker",
            OpenParen => "parenthesis",
            CloseParen => "parenthesis end",
            OpenCurly => "open curly brace",
            CloseCurly => "close curly brace",
            OpenList => "start of list",
            CloseList => "end of list",
            Attributes => "attributes header",
            BoolTrue => "boolean value",
            BoolFalse => "boolean value",
            TypeSelfStrict => "self marker",
            Comma => "comma",
            Arrow => "arrow",
            FatArrow => "fat arrow",
            Next => "next",
            PipeLeft => "left pipe",
            PipeRight => "right pipe",
            Let => "start of let binding",
            In => "end of let assignment",
            Mut => "mutability flag",
            Debug => "debug flag",
            DoubleColon => "double colon",
            First => "start of first statement",
            Lambda => "start of lambda",
            Pass => "start of closure",
            Underscore => "underscore",
            HeaderFn => "function header",
            HeaderStruct => "struct header",
            HeaderEnum => "enum header",
            HeaderTrait => "trait header",
            HeaderUse => "use header",
            HeaderImpl => "impl header",
            Error => "token error",

            EOF => "end of file",
        }
    }

    /// Whether this token is useful to portray to the user in error
    pub fn is_showable(&self) -> bool {
        use Token::*;

        matches!(
            self,
            Int(_) | Float(_) | StringLiteral | Identifier | Operator | BoolTrue | BoolFalse
        )
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.describe().fmt(f)
    }
}
