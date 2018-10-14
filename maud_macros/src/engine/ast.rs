use proc_macro::{Span, TokenStream, TokenTree};

#[derive(Debug)]
pub struct Block<C> {
    pub contents: Vec<C>,
    pub outer_span: Span,
}

impl<C> Block<C> {
    pub fn span(&self) -> Span {
        self.outer_span
    }
}
    
#[derive(Debug)]
pub struct Literal {
    pub content: String,
    pub span: Span,
}

impl Literal {
    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug)]
pub struct Splice {
    pub expr: TokenStream,
    pub outer_span: Span,
}

impl Splice {
    pub fn span(&self) -> Span {
        self.outer_span
    }
}

#[derive(Debug)]
pub enum Command<C> {
    Let {
        at_span: Span,
        tokens: TokenStream,
    },
    Special {
        segments: Vec<Special<C>>,
    },
    Match {
        at_span: Span,
        head: TokenStream,
        arms: Vec<MatchArm<C>>,
        arms_span: Span,
    },
}


impl<C> Command<C> {
    pub fn span(&self) -> Span {
        match *self {
            Command::Let { at_span, ref tokens } => {
                at_span.join(span_tokens(tokens.clone())).unwrap_or(at_span)
            },
            Command::Special { ref segments } => {
                join_spans(segments.iter().map(|segment| segment.span()))
            },
            Command::Match { at_span, arms_span, .. } => {
                at_span.join(arms_span).unwrap_or(at_span)
            },
        }
    }
}

#[derive(Debug)]
pub struct Special<C> {
    pub at_span: Span,
    pub head: TokenStream,
    pub body: Block<C>,
}

impl<C> Special<C> {
    pub fn span(&self) -> Span {
        let body_span = self.body.span();
        self.at_span.join(body_span).unwrap_or(self.at_span)
    }
}

#[derive(Debug)]
pub struct MatchArm<C> {
    pub head: TokenStream,
    pub body: Block<C>,
}

pub fn span_tokens<I: IntoIterator<Item=TokenTree>>(tokens: I) -> Span {
    join_spans(tokens.into_iter().map(|token| token.span()))
}

pub fn join_spans<I: IntoIterator<Item=Span>>(spans: I) -> Span {
    let mut iter = spans.into_iter();
    let mut span = match iter.next() {
        Some(span) => span,
        None => return Span::call_site(),
    };
    for new_span in iter {
        if let Some(joined) = span.join(new_span) {
            span = joined;
        }
    }
    span
}
