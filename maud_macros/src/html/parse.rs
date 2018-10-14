use proc_macro::{
    Delimiter,
    Literal,
    Span,
    TokenStream,
    TokenTree,
};
use std::collections::HashMap;
use std::mem;

use literalext::LiteralExt;

use engine::parse::{Parser as EngineParser};
use html::ast;
use ParseResult;

pub fn parse(input: TokenStream) -> ParseResult<Vec<ast::Markup>> {
    Parser::new(input).markups()
}

#[derive(Clone)]
struct Parser {
    /// Indicates whether we're inside an attribute node.
    in_attr: bool,
    input: <TokenStream as IntoIterator>::IntoIter,
}

impl Iterator for Parser {
    type Item = TokenTree;

    fn next(&mut self) -> Option<TokenTree> {
        self.input.next()
    }
}

impl Parser {
    fn new(input: TokenStream) -> Parser {
        Parser {
            in_attr: false,
            input: input.into_iter(),
        }
    }

    fn markups(&mut self) -> ParseResult<Vec<ast::Markup>> {
        self.contents()
    }

    fn markup(&mut self) -> ParseResult<ast::Markup> {
        self.content()
    }
}

impl EngineParser for Parser {
    type Content = ast::Markup;

    fn with_input(&self, input: TokenStream) -> Parser {
        Parser {
            in_attr: self.in_attr,
            input: input.into_iter(),
        }
    }

    fn next(&mut self) -> Option<TokenTree> {
        <Self as Iterator>::next(self)
    }

    fn attempt(&mut self) -> Parser {
        self.clone()
    }

    /// Overwrites the current parser state with the given parameter.
    fn commit(&mut self, attempt: Parser) {
        *self = attempt;
    }

    /// Parses and renders a single block of markup.
    fn content(&mut self) -> ParseResult<ast::Markup> {
        let token = match self.peek() {
            Some(token) => token,
            None => {
                Span::call_site().error("unexpected end of input").emit();
                return Err(());
            },
        };
        let markup = match token {
            // Literal
            TokenTree::Literal(lit) => {
                self.advance();
                self.literal(&lit)?
            },
            // Special form
            TokenTree::Punct(ref punct) if punct.as_char() == '@' => {
                self.advance();
                let at_span = punct.span();
                match <Self as EngineParser>::next(self) {
                    Some(TokenTree::Ident(ident)) => {
                        let keyword = TokenTree::Ident(ident.clone());
                        let command = match ident.to_string().as_str() {
                            "if" => self.if_expr(at_span, keyword)?,
                            "while" => self.while_expr(at_span, keyword)?,
                            "for" => self.for_expr(at_span, keyword)?,
                            "match" => self.match_expr(at_span, keyword)?,
                            "let" => {
                                let ident_span = ident.span();
                                let span = at_span.join(ident_span).unwrap_or(ident_span);
                                span.error("`@let` only works inside a block").emit();
                                self.let_expr(at_span, keyword)?
                            },
                            other => {
                                let ident_span = ident.span();
                                let span = at_span.join(ident_span).unwrap_or(ident_span);
                                span.error(format!("unknown keyword `@{}`", other)).emit();
                                return Err(());
                            }
                        };
                        ast::Markup::from(command)
                    },
                    _ => {
                        at_span.error("expected keyword after `@`").emit();
                        return Err(());
                    },
                }
            },
            // Element
            TokenTree::Ident(_) => {
                // `.try_namespaced_name()` should never fail as we've
                // already seen an `Ident`
                let name = self.try_namespaced_name().expect("identifier");
                self.element(name)?
            },
            // Splice
            TokenTree::Group(ref group) if group.delimiter() == Delimiter::Parenthesis => {
                self.advance();
                ast::Markup::Splice(ast::Splice { expr: group.stream(), outer_span: group.span() })
            }
            // Block
            TokenTree::Group(ref group) if group.delimiter() == Delimiter::Brace => {
                self.advance();
                ast::Markup::Block(self.block(group.stream(), group.span())?)
            },
            // ???
            token => {
                token.span().error("invalid syntax").emit();
                return Err(());
            },
        };
        Ok(markup)
    }
    
    /// Parses and renders multiple blocks of markup.
    fn contents(&mut self) -> ParseResult<Vec<ast::Markup>> {
        let mut result = Vec::new();
        loop {
            match self.peek2() {
                None => break,
                Some((TokenTree::Punct(ref punct), _)) if punct.as_char() == ';' => self.advance(),
                Some((
                    TokenTree::Punct(ref punct),
                    Some(TokenTree::Ident(ref ident)),
                )) if punct.as_char() == '@' && ident.to_string() == "let" => {
                    self.advance2();
                    let keyword = TokenTree::Ident(ident.clone());
                    result.push(ast::Markup::from(self.let_expr(punct.span(), keyword)?));
                },
                _ => result.push(self.content()?),
            }
        }
        Ok(result)
    }
}

impl Parser {
    /// Parses and renders a literal string.
    fn literal(&mut self, lit: &Literal) -> ParseResult<ast::Markup> {
        let content = lit.parse_string().unwrap_or_else(|| {
            lit.span().error("expected string").emit();
            String::new()  // Insert a dummy value
        });
        Ok(ast::Markup::Literal(ast::Literal {
            content,
            span: lit.span(),
        }))
    }

    /// Parses an element node.
    ///
    /// The element name should already be consumed.
    fn element(&mut self, name: TokenStream) -> ParseResult<ast::Markup> {
        if self.in_attr {
            let span = ast::span_tokens(name);
            span.error("unexpected element, you silly bumpkin").emit();
            return Err(());
        }
        let attrs = self.attrs()?;
        let body = match self.peek() {
            Some(TokenTree::Punct(ref punct))
            if punct.as_char() == ';' || punct.as_char() == '/' => {
                // Void element
                self.advance();
                ast::ElementBody::Void { semi_span: punct.span() }
            },
            _ => {
                match self.markup()? {
                    ast::Markup::Block(block) => ast::ElementBody::Block { block },
                    markup => {
                        let markup_span = markup.span();
                        markup_span
                            .error("element body must be wrapped in braces")
                            .help("see https://github.com/lfairy/maud/pull/137 for details")
                            .emit();
                        ast::ElementBody::Block {
                            block: ast::Block {
                                contents: vec![markup],
                                outer_span: markup_span,
                            },
                        }
                    },
                }
            },
        };
        Ok(ast::Markup::Element { name, attrs, body })
    }

    /// Parses the attributes of an element.
    fn attrs(&mut self) -> ParseResult<ast::Attrs> {
        let mut attrs = Vec::new();
        loop {
            let mut attempt = self.clone();
            let maybe_name = attempt.try_namespaced_name();
            let token_after = <Self as EngineParser>::next(&mut attempt);
            match (maybe_name, token_after) {
                // Non-empty attribute
                (Some(ref name), Some(TokenTree::Punct(ref punct))) if punct.as_char() == '=' => {
                    self.commit(attempt);
                    let value;
                    {
                        // Parse a value under an attribute context
                        let in_attr = mem::replace(&mut self.in_attr, true);
                        value = self.markup()?;
                        self.in_attr = in_attr;
                    }
                    attrs.push(ast::Attr::Attribute {
                        attribute: ast::Attribute {
                            name: name.clone(),
                            attr_type: ast::AttrType::Normal { value },
                        },
                    });
                },
                // Empty attribute
                (Some(ref name), Some(TokenTree::Punct(ref punct))) if punct.as_char() == '?' => {
                    self.commit(attempt);
                    let toggler = self.attr_toggler();
                    attrs.push(ast::Attr::Attribute {
                        attribute: ast::Attribute {
                            name: name.clone(),
                            attr_type: ast::AttrType::Empty { toggler },
                        },
                    });
                },
                // Class shorthand
                (None, Some(TokenTree::Punct(ref punct))) if punct.as_char() == '.' => {
                    self.commit(attempt);
                    let name = self.class_or_id_name()?;
                    let toggler = self.attr_toggler();
                    attrs.push(ast::Attr::Class { dot_span: punct.span(), name, toggler });
                },
                // ID shorthand
                (None, Some(TokenTree::Punct(ref punct))) if punct.as_char() == '#' => {
                    self.commit(attempt);
                    let name = self.class_or_id_name()?;
                    attrs.push(ast::Attr::Id { hash_span: punct.span(), name });
                },
                // If it's not a valid attribute, backtrack and bail out
                _ => break,
            }
        }

        let mut attr_map: HashMap<String, Vec<Span>> = HashMap::new();
        let mut has_class = false;
        for attr in &attrs {
            let name = match attr {
                ast::Attr::Class { .. } => {
                    if has_class {
                        // Only check the first class to avoid spurious duplicates
                        continue;
                    }
                    has_class = true;
                    "class".to_string()
                },
                ast::Attr::Id { .. } => "id".to_string(),
                ast::Attr::Attribute { attribute } => {
                    attribute.name.clone().into_iter().map(|token| token.to_string()).collect()
                },
            };
            let entry = attr_map.entry(name).or_default();
            entry.push(attr.span());
        }

        for (name, spans) in attr_map {
            if spans.len() > 1 {
                let mut spans = spans.into_iter();
                let first_span = spans.next().expect("spans should be non-empty");
                spans
                    .fold(
                        first_span.error(format!("duplicate attribute `{}`", name)),
                        |acc, span| acc.span_note(span, format!("`{}` is duplicated here", name)),
                    )
                    .emit();
            }
        }

        Ok(attrs)
    }

    /// Parses the name of a class or ID.
    fn class_or_id_name(&mut self) -> ParseResult<ast::Markup> {
        if let Some(symbol) = self.try_name() {
            Ok(ast::Markup::Symbol { symbol })
        } else {
            self.markup()
        }
    }

    /// Parses the `[cond]` syntax after an empty attribute or class shorthand.
    fn attr_toggler(&mut self) -> Option<ast::Toggler> {
        match self.peek() {
            Some(TokenTree::Group(ref group)) if group.delimiter() == Delimiter::Bracket => {
                self.advance();
                Some(ast::Toggler {
                    cond: group.stream(),
                    cond_span: group.span(),
                })
            },
            _ => None,
        }
    }

    /// Parses an identifier, without dealing with namespaces.
    fn try_name(&mut self) -> Option<TokenStream> {
        let mut result = Vec::new();
        if let Some(token @ TokenTree::Ident(_)) = self.peek() {
            self.advance();
            result.push(token);
        } else {
            return None;
        }
        let mut expect_ident = false;
        loop {
            expect_ident = match self.peek() {
                Some(TokenTree::Punct(ref punct)) if punct.as_char() == '-' => {
                    self.advance();
                    result.push(TokenTree::Punct(punct.clone()));
                    true
                },
                Some(TokenTree::Ident(ref ident)) if expect_ident => {
                    self.advance();
                    result.push(TokenTree::Ident(ident.clone()));
                    false
                },
                _ => break,
            };
        }
        Some(result.into_iter().collect())
    }

    /// Parses a HTML element or attribute name, along with a namespace
    /// if necessary.
    fn try_namespaced_name(&mut self) -> Option<TokenStream> {
        let mut result = vec![self.try_name()?];
        if let Some(TokenTree::Punct(ref punct)) = self.peek() {
            if punct.as_char() == ':' {
                self.advance();
                result.push(TokenStream::from(TokenTree::Punct(punct.clone())));
                result.push(self.try_name()?);
            }
        }
        Some(result.into_iter().collect())
    }
}
