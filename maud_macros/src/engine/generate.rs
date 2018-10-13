use proc_macro::{
    Delimiter,
    Group,
    quote,
    TokenStream,
    TokenTree,
};

use engine::ast::*;

pub trait Generator {
    type Content;
    type Builder: Builder;

    fn builder(&self) -> Self::Builder;
    
    fn output_ident(&self) -> TokenTree;
    
    fn contents(&self, contents: Vec<Self::Content>, build: &mut Self::Builder) {
        for content in contents {
            self.content(content, build);
        }
    }

    fn content(&self, content: Self::Content, build: &mut Self::Builder);
    
    fn block(&self, block: Block<Self::Content>) -> TokenStream {
        let Block { contents, outer_span } = block;
        let mut build = self.builder();
        self.contents(contents, &mut build);
        let mut block = TokenTree::Group(Group::new(Delimiter::Brace, build.finish()));
        block.set_span(outer_span);
        TokenStream::from(block)
    }

    fn splice(&self, splice: Splice) -> TokenStream {
        let Splice { expr, .. } = splice;
        let output_ident = self.output_ident();
        quote!({
            // Create a local trait alias so that autoref works
            trait Render: maud::Render {
                fn __maud_render_to(&self, output_ident: &mut String) {
                    maud::Render::render_to(self, output_ident);
                }
            }
            impl<T: maud::Render> Render for T {}
            $expr.__maud_render_to(&mut $output_ident);
        })
    }
    
    fn command(&self, command: Command<Self::Content>, build: &mut Self::Builder) {
        match command {
            Command::Let { tokens, .. } => build.push_tokens(tokens),
            Command::Special { segments } => {
                for segment in segments {
                    build.push_tokens(self.special(segment));
                }
            },
            Command::Match { head, arms, arms_span, .. } => {
                build.push_tokens({
                    let body = arms
                        .into_iter()
                        .map(|arm| self.match_arm(arm))
                        .collect();
                    let mut body = TokenTree::Group(Group::new(Delimiter::Brace, body));
                    body.set_span(arms_span);
                    quote!($head $body)
                });
            },
        }
    }

    fn special(&self, special: Special<Self::Content>) -> TokenStream {
        let Special { head, body, ..} = special;
        let body = self.block(body);
        quote!($head $body)
    }

    fn match_arm(&self, match_arm: MatchArm<Self::Content>) -> TokenStream {
        let MatchArm { head, body } = match_arm;
        let body = self.block(body);
        quote!($head $body)
    }
}

////////////////////////////////////////////////////////

pub trait Builder {
    fn push_str(&mut self, string: &str);

    fn push_tokens<T: IntoIterator<Item=TokenTree>>(&mut self, tokens: T);

    fn finish(self) -> TokenStream;
}
