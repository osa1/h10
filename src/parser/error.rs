use crate::layout_lexer::LayoutLexer_;
use crate::parser::{Parser, ParserResult};

use lexgen_util::Loc;
use rpds::List;

// TODO: Should errors have reference to the file/module?
// TODO: Add unexpected token kind and expected kinds
#[derive(Debug)]
pub struct Error {
    // Location is not valid when `kind` is `UnexpectedEndOfInput`
    loc: Loc,
    #[allow(unused)]
    kind: ErrorKind,
    context: List<Context>,

    #[cfg(debug_assertions)]
    backtrace: std::backtrace::Backtrace,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorKind {
    UnexpectedToken,
    LexerError,
    UnexpectedEndOfInput,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Context {
    pub loc: Loc,
    pub item: GrammarItem,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GrammarItem {
    Module,
    ExportList,
    TopDecls,
}

impl<'input, L: LayoutLexer_> Parser<'input, L> {
    pub fn in_context<R, F>(&mut self, item: GrammarItem, mut f: F) -> ParserResult<R>
    where
        F: FnMut(&mut Self) -> ParserResult<R>,
    {
        let context_size = self.context.len();
        let next_token_loc = self.peek()?.0;
        let context = Context {
            loc: next_token_loc,
            item,
        };
        self.context.push_front_mut(context.clone());
        let ret = f(self);
        debug_assert_eq!(self.context.first(), Some(&context));
        self.context.drop_first_mut();
        debug_assert_eq!(self.context.len(), context_size);
        ret
    }

    pub fn fail<R>(&mut self, loc: Loc, kind: ErrorKind) -> ParserResult<R> {
        Err(Error {
            loc,
            kind,
            context: self.context.clone(),
            #[cfg(debug_assertions)]
            backtrace: std::backtrace::Backtrace::capture(),
        })
    }

    pub fn fail_with_next<R>(&mut self, kind: ErrorKind) -> ParserResult<R> {
        let (loc, _, _) = self.peek()?;
        Err(Error {
            loc,
            kind,
            context: self.context.clone(),
            #[cfg(debug_assertions)]
            backtrace: std::backtrace::Backtrace::capture(),
        })
    }
}

use std::fmt;

impl fmt::Display for GrammarItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            GrammarItem::Module => "module",
            GrammarItem::ExportList => "export list",
            GrammarItem::TopDecls => "top-level declarations",
        };
        f.write_str(s)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Parse error at {}:{}", self.loc.line, self.loc.col)?;
        if !self.context.is_empty() {
            writeln!(f, "While parsing:")?;
            for Context { loc, item } in &self.context {
                writeln!(f, "  - {} at {}:{}", item, loc.line, loc.col)?;
            }
        }
        #[cfg(debug_assertions)]
        {
            writeln!(f)?;
            writeln!(f, "Error backtrace:")?;
            // TODO: Stable API doesn't give access to the frames, we have to parse Display output
            // for now.
            let backtrace_str = self.backtrace.to_string();
            let backtrace_lines = backtrace_str.lines();
            for line in backtrace_lines {
                if line.contains("h10::main") {
                    break;
                }
                writeln!(f, "{}", line)?;
            }
        }
        // TODO: kind
        Ok(())
    }
}
