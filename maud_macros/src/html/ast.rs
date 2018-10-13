use engine;
pub use engine::ast::{Literal, Splice, Command, span_tokens};

pub type Block = engine::ast::Block<Markup>;
// Type aliasing an enum doesn't give proper access to the values (RFC #2338),
// so this, while desirable, isn't possible
//pub type Command = engine::ast::Command<Markup>;
pub type Special = engine::ast::Special<Markup>;
pub type MatchArm = engine::ast::MatchArm<Markup>;

use proc_macro::{Span, TokenStream};

#[derive(Debug)]
pub enum Markup {
    Block(Block),
    Literal(Literal),
    Symbol {
        symbol: TokenStream,
    },
    Splice(Splice),
    Element {
        name: TokenStream,
        attrs: Attrs,
        body: ElementBody,
    },
    Command(Command<Markup>),
}

impl Markup {
    pub fn span(&self) -> Span {
        match *self {
            Markup::Block(ref block) => block.span(),
            Markup::Literal(ref literal) => literal.span(),
            Markup::Symbol { ref symbol } => span_tokens(symbol.clone()),
            Markup::Splice(ref splice) => splice.span(),
            Markup::Element { ref name, ref body, .. } => {
                let name_span = span_tokens(name.clone());
                name_span.join(body.span()).unwrap_or(name_span)
            },
            Markup::Command(ref command) => command.span(),
        }
    }
}

pub type Attrs = Vec<Attr>;

#[derive(Debug)]
pub enum Attr {
    Class {
        dot_span: Span,
        name: Markup,
        toggler: Option<Toggler>,
    },
    Id {
        hash_span: Span,
        name: Markup,
    },
    Attribute {
        attribute: Attribute,
    },
}

impl Attr {
    pub fn span(&self) -> Span {
        match *self {
            Attr::Class { dot_span, ref name, ref toggler } => {
                let name_span = name.span();
                let dot_name_span = dot_span.join(name_span).unwrap_or(dot_span);
                if let Some(toggler) = toggler {
                    dot_name_span.join(toggler.cond_span).unwrap_or(name_span)
                } else {
                    dot_name_span
                }
            },
            Attr::Id { hash_span, ref name } => {
                let name_span = name.span();
                hash_span.join(name_span).unwrap_or(hash_span)
            },
            Attr::Attribute { ref attribute } => attribute.span(),
        }
    }
}

#[derive(Debug)]
pub enum ElementBody {
    Void { semi_span: Span },
    Block { block: Block },
}

impl ElementBody {
    pub fn span(&self) -> Span {
        match *self {
            ElementBody::Void { semi_span } => semi_span,
            ElementBody::Block { ref block } => block.span(),
        }
    }
}

#[derive(Debug)]
pub struct Attribute {
    pub name: TokenStream,
    pub attr_type: AttrType,
}

impl Attribute {
    fn span(&self) -> Span {
        let name_span = span_tokens(self.name.clone());
        if let Some(attr_type_span) = self.attr_type.span() {
            name_span.join(attr_type_span).unwrap_or(name_span)
        } else {
            name_span
        }
    }
}

#[derive(Debug)]
pub enum AttrType {
    Normal {
        value: Markup,
    },
    Empty {
        toggler: Option<Toggler>,
    },
}

impl AttrType {
    fn span(&self) -> Option<Span> {
        match *self {
            AttrType::Normal { ref value } => Some(value.span()),
            AttrType::Empty { ref toggler } => toggler.as_ref().map(|toggler| toggler.span()),
        }
    }
}

#[derive(Debug)]
pub struct Toggler {
    pub cond: TokenStream,
    pub cond_span: Span,
}

impl Toggler {
    fn span(&self) -> Span {
        self.cond_span
    }
}

