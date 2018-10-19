use proc_macro::{
    Delimiter,
    Spacing,
    Span,
    TokenStream,
    TokenTree,
    Group,
};

use engine::ast;
use ParseResult;

pub trait Parser : Sized {
    type Content;
    
    fn with_input(&self, input: TokenStream) -> Self;

    fn peek(&mut self) -> Option<TokenTree> {
        // Given attempt() and next(), we can provide a default implementation
        // of peek()
        self.attempt().next()
    }
    fn peek2(&mut self) -> Option<(TokenTree, Option<TokenTree>)> {
        // Given peek() and attempt(), we can provide a default implementation
        // of peek2()
        let mut next = {
            let mut clone = self.attempt();
            clone.advance();
            clone
        };
        self.peek().map(|tt| (tt, next.peek()))
    }

    fn advance(&mut self) {
        // Given next(), we can provide a default implementation of advance(),
        // where we discard the value
        self.next();
    }
    fn advance2(&mut self) {
        self.advance();
        self.advance();
    }

    fn next(&mut self) -> Option<TokenTree>;

    fn attempt(&mut self) -> Self;
    fn commit(&mut self, attempt: Self);
    
    /// Parses and renders multiple blocks of markup.
    fn contents(&mut self) -> ParseResult<Vec<Self::Content>>;
    fn content(&mut self) -> ParseResult<Self::Content>;

    /// Parses a template command, like loops and assignments
    ///
    /// Any command marker, like a `@`, should already be consumed
    fn command(&mut self, at_span: Span) -> ParseResult<ast::Command<Self::Content>> {
        match self.next() {
            Some(TokenTree::Ident(ident)) => {
                let keyword = TokenTree::Ident(ident.clone());
                match ident.to_string().as_str() {
                    "if" => self.if_expr(at_span, keyword),
                    "while" => self.while_expr(at_span, keyword),
                    "for" => self.for_expr(at_span, keyword),
                    "match" => self.match_expr(at_span, keyword),
                    "let" => self.let_expr(at_span, keyword),
                    other => {
                        let ident_span = ident.span();
                        let span = at_span.join(ident_span).unwrap_or(ident_span);
                        span.error(format!("unknown keyword `@{}`", other)).emit();
                        Err(())
                    }
                }
            },
            _ => {
                at_span.error("expected keyword after `@`").emit();
                Err(())
            },
        }
    }

    /// Parses a `@let` expression.
    ///
    /// The leading `@let` should already be consumed.
    fn let_expr(&mut self, at_span: Span, keyword: TokenTree) -> ParseResult<ast::Command<Self::Content>> {
        let mut tokens = vec![keyword];
        loop {
            match self.next() {
                Some(token) => {
                    match token {
                        TokenTree::Punct(ref punct) if punct.as_char() == '=' => {
                            tokens.push(token.clone());
                            break;
                        },
                        _ => tokens.push(token),
                    }
                },
                None => {
                    let tokens_span = ast::span_tokens(tokens);
                    let span = at_span.join(tokens_span).unwrap_or(tokens_span);
                    span.error("unexpected end of `@let` expression").emit();
                    return Err(());
                }
            }
        }
        loop {
            match self.next() {
                Some(token) => {
                    match token {
                        TokenTree::Punct(ref punct) if punct.as_char() == ';' => {
                            tokens.push(token.clone());
                            break;
                        },
                        _ => tokens.push(token),
                    }
                },
                None => {
                    let tokens_span = ast::span_tokens(tokens);
                    let span = at_span.join(tokens_span).unwrap_or(tokens_span);
                    span.error("unexpected end of `@let` expression (are you missing a semicolon?)")
                        .emit();
                    return Err(());
                },
            }
        }
        Ok(ast::Command::Let { at_span, tokens: tokens.into_iter().collect() })
    }

    fn if_expr(
        &mut self,
        at_span: Span,
        keyword: TokenTree,
    ) -> ParseResult<ast::Command<Self::Content>> {
        let mut segments = vec![];
        self.if_head_expr(at_span, vec![keyword], &mut segments)?;
        Ok(ast::Command::Special { segments })
    }
    
    /// Parses an `@if` expression.
    ///
    /// The leading `@if` should already be consumed.
    fn if_head_expr(
        &mut self,
        at_span: Span,
        prefix: Vec<TokenTree>,
        segments: &mut Vec<ast::Special<Self::Content>>,
    ) -> ParseResult<()> {
        let mut head = prefix;
        let body = loop {
            match self.next() {
                Some(TokenTree::Group(ref block)) if block.delimiter() == Delimiter::Brace => {
                    break self.block(block)?;
                },
                Some(token) => head.push(token),
                None => {
                    let head_span = ast::span_tokens(head);
                    let span = at_span.join(head_span).unwrap_or(head_span);
                    span.error("expected body for this `@if`").emit();
                    return Err(());
                },
            }
        };
        segments.push(ast::Special {
            at_span,
            head: head.into_iter().collect(),
            body,
        });
        self.else_if_expr(segments)
    }
    
    /// Parses an optional `@else if` or `@else`.
    ///
    /// The leading `@else if` or `@else` should *not* already be consumed.
    fn else_if_expr(&mut self, segments: &mut Vec<ast::Special<Self::Content>>) -> ParseResult<()> {
        match self.peek2() {
            Some((
                TokenTree::Punct(ref punct),
                Some(TokenTree::Ident(ref else_keyword)),
            )) if punct.as_char() == '@' && else_keyword.to_string() == "else" => {
                self.advance2();
                let at_span = punct.span();
                let else_keyword = TokenTree::Ident(else_keyword.clone());
                match self.peek() {
                    // `@else if`
                    Some(TokenTree::Ident(ref if_keyword)) if if_keyword.to_string() == "if" => {
                        self.advance();
                        let if_keyword = TokenTree::Ident(if_keyword.clone());
                        self.if_head_expr(at_span, vec![else_keyword, if_keyword], segments)
                    },
                    // Just an `@else`
                    _ => {
                        match self.next() {
                            Some(TokenTree::Group(ref group)) if group.delimiter() == Delimiter::Brace => {
                                let body = self.block(group)?;
                                segments.push(ast::Special {
                                    at_span,
                                    head: vec![else_keyword].into_iter().collect(),
                                    body,
                                });
                                Ok(())
                            },
                            _ => {
                                let else_span = else_keyword.span();
                                let span = at_span.join(else_span).unwrap_or(else_span);
                                span.error("expected body for this `@else`").emit();
                                Err(())
                            },
                        }
                    },
                }
            },
            // We didn't find an `@else`; stop
            _ => Ok(()),
        }
    }

    /// Parses and renders an `@while` expression.
    ///
    /// The leading `@while` should already be consumed.
    fn while_expr(&mut self, at_span: Span, keyword: TokenTree) -> ParseResult<ast::Command<Self::Content>> {
        let keyword_span = keyword.span();
        let mut head = vec![keyword];
        let body = loop {
            match self.next() {
                Some(TokenTree::Group(ref block)) if block.delimiter() == Delimiter::Brace => {
                    break self.block(block)?;
                },
                Some(token) => head.push(token),
                None => {
                    let span = at_span.join(keyword_span).unwrap_or(keyword_span);
                    span.error("expected body for this `@while`").emit();
                    return Err(());
                },
            }
        };
        Ok(ast::Command::Special {
            segments: vec![ast::Special { at_span, head: head.into_iter().collect(), body }],
        })
    }

    /// Parses a `@for` expression.
    ///
    /// The leading `@for` should already be consumed.
    fn for_expr(&mut self, at_span: Span, keyword: TokenTree) -> ParseResult<ast::Command<Self::Content>> {
        let keyword_span = keyword.span();
        let mut head = vec![keyword];
        loop {
            match self.next() {
                Some(TokenTree::Ident(ref in_keyword)) if in_keyword.to_string() == "in" => {
                    head.push(TokenTree::Ident(in_keyword.clone()));
                    break;
                },
                Some(token) => head.push(token),
                None => {
                    let span = at_span.join(keyword_span).unwrap_or(keyword_span);
                    span.error("missing `in` in `@for` loop").emit();
                    return Err(());
                },
            }
        }
        let body = loop {
            match self.next() {
                Some(TokenTree::Group(ref block)) if block.delimiter() == Delimiter::Brace => {
                    break self.block(block)?;
                },
                Some(token) => head.push(token),
                None => {
                    let span = at_span.join(keyword_span).unwrap_or(keyword_span);
                    span.error("expected body for this `@for`").emit();
                    return Err(());
                },
            }
        };
        Ok(ast::Command::Special {
            segments: vec![ast::Special { at_span, head: head.into_iter().collect(), body }],
        })
    }
    
    /// Parses a `@match` expression.
    ///
    /// The leading `@match` should already be consumed.
    fn match_expr(&mut self, at_span: Span, keyword: TokenTree) -> ParseResult<ast::Command<Self::Content>> {
        let keyword_span = keyword.span();
        let mut head = vec![keyword];
        let (arms, arms_span) = loop {
            match self.next() {
                Some(TokenTree::Group(ref body)) if body.delimiter() == Delimiter::Brace => {
                    let span = body.span();
                    break (self.with_input(body.stream()).match_arms()?, span);
                },
                Some(token) => head.push(token),
                None => {
                    let span = at_span.join(keyword_span).unwrap_or(keyword_span);
                    span.error("expected body for this `@match`").emit();
                    return Err(());
                },
            }
        };
        Ok(ast::Command::Match { 
            at_span, head: head.into_iter().collect(), arms, arms_span 
        })
    }
    
    fn match_arms(&mut self) -> ParseResult<Vec<ast::MatchArm<Self::Content>>> {
        let mut arms = Vec::new();
        while let Some(arm) = self.match_arm()? {
            arms.push(arm);
        }
        Ok(arms)
    }

    fn match_arm(&mut self) -> ParseResult<Option<ast::MatchArm<Self::Content>>> {
        let mut head = Vec::new();
        loop {
            match self.peek2() {
                Some((TokenTree::Punct(ref eq), Some(TokenTree::Punct(ref gt))))
                if eq.as_char() == '=' && gt.as_char() == '>' && eq.spacing() == Spacing::Joint => {
                    self.advance2();
                    head.push(TokenTree::Punct(eq.clone()));
                    head.push(TokenTree::Punct(gt.clone()));
                    break;
                },
                Some((token, _)) => {
                    self.advance();
                    head.push(token);
                },
                None => {
                    if head.is_empty() {
                        return Ok(None);
                    } else {
                        let head_span = ast::span_tokens(head);
                        head_span.error("unexpected end of @match pattern").emit();
                        return Err(());
                    }
                },
            }
        }
        let body = match self.next() {
            // $pat => { $stmts }
            Some(TokenTree::Group(ref body)) if body.delimiter() == Delimiter::Brace => {
                let body = self.block(body)?;
                // Trailing commas are optional if the match arm is a braced block
                if let Some(TokenTree::Punct(ref punct)) = self.peek() {
                    if punct.as_char() == ',' {
                        self.advance();
                    }
                }
                body
            },
            // $pat => $expr
            Some(first_token) => {
                let mut span = first_token.span();
                let mut body = vec![first_token];
                loop {
                    match self.next() {
                        Some(TokenTree::Punct(ref punct)) if punct.as_char() == ',' => break,
                        Some(token) => {
                            if let Some(bigger_span) = span.join(token.span()) {
                                span = bigger_span;
                            }
                            body.push(token);
                        },
                        None => break,
                    }
                }
                self.make_block(body.into_iter().collect(), span)?
            },
            None => {
                let span = ast::span_tokens(head);
                span.error("unexpected end of @match arm").emit();
                return Err(());
            },
        };
        Ok(Some(ast::MatchArm { head: head.into_iter().collect(), body }))
    }

    fn block(&mut self, group: &Group) -> ParseResult<ast::Block<Self::Content>> {
        let body = group.stream();
        let outer_span = group.span();
        self.make_block(body, outer_span)
    }

    fn make_block(&mut self, body: TokenStream, outer_span: Span) -> ParseResult<ast::Block<Self::Content>> {
        let contents = self.with_input(body).contents()?;
        Ok(ast::Block { contents, outer_span })
    }
    
    fn splice(&mut self, group: &Group) -> ast::Splice {
        let expr = group.stream();
        let outer_span = group.span();
        ast::Splice { expr, outer_span }
    }
}
