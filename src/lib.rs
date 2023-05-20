#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[rustfmt::skip]
enum TokenKind {
  ErrorToken, Eof,

  LParen, RParen, LCurly, RCurly,
  Eq, Semi, Comma, Colon, Arrow,
  Plus, Minus, Star, Slash,

  FnKeyword, LetKeyword, ReturnKeyword,
  TrueKeyword, FalseKeyword,

  Name, Int,
}

#[derive(Debug)]
#[rustfmt::skip]
enum TreeKind {
  ErrorTree, File,
  Fn, TypeExpr,
  ParamList, Param,
  Block,
  StmtLet, StmtReturn, StmtExpr,
  ExprLiteral, ExprName, ExprParen, ExprBinary, ExprCall,
  ArgList, Arg,
}

#[derive(Debug)]
struct Token {
  kind: TokenKind,
  text: String,
}

pub struct Tree {
  kind: TreeKind,
  children: Vec<Child>,
}

enum Child {
  Token(Token),
  Tree(Tree),
}

pub fn parse(text: &str) -> Tree {
  let tokens = lex(text);
  let mut p = Parser::new(tokens);
  file(&mut p);
  p.build_tree()
}

#[macro_export]
macro_rules! format_to {
    ($buf:expr) => ();
    ($buf:expr, $lit:literal $($arg:tt)*) => {
        { use ::std::fmt::Write as _; let _ = ::std::write!($buf, $lit $($arg)*); }
    };
}

impl Tree {
  fn print(&self, buf: &mut String, level: usize) {
    let indent = "  ".repeat(level);
    format_to!(buf, "{indent}{:?}\n", self.kind);
    for child in &self.children {
      match child {
        Child::Token(token) => {
          format_to!(buf, "{indent}  '{}'\n", token.text)
        }
        Child::Tree(tree) => tree.print(buf, level + 1),
      }
    }
    assert!(buf.ends_with('\n'));
  }
}

impl fmt::Debug for Tree {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut buf = String::new();
    self.print(&mut buf, 0);
    write!(f, "{}", buf)
  }
}

fn lex(mut text: &str) -> Vec<Token> {
  let punctuation = (
    "( ) { } = ; , : -> + - * /",
    [
      LParen, RParen, LCurly, RCurly, Eq, Semi, Comma, Colon,
      Arrow, Plus, Minus, Star, Slash,
    ],
  );

  let keywords = (
    "fn let return true false",
    [
      FnKeyword,
      LetKeyword,
      ReturnKeyword,
      TrueKeyword,
      FalseKeyword,
    ],
  );

  let mut result = Vec::new();
  while !text.is_empty() {
    if let Some(rest) = trim(text, |it| it.is_ascii_whitespace())
    {
      text = rest;
      continue;
    }
    let text_orig = text;
    let mut kind = 'kind: {
      for (i, symbol) in
        punctuation.0.split_ascii_whitespace().enumerate()
      {
        if let Some(rest) = text.strip_prefix(symbol) {
          text = rest;
          break 'kind punctuation.1[i];
        }
      }
      if let Some(rest) = trim(text, |it| it.is_ascii_digit()) {
        text = rest;
        break 'kind Int;
      }
      if let Some(rest) = trim(text, name_char) {
        text = rest;
        break 'kind Name;
      }
      let error_index = text
        .find(|it: char| it.is_ascii_whitespace())
        .unwrap_or(text.len());
      text = &text[error_index..];
      ErrorToken
    };
    assert!(text.len() < text_orig.len());
    let token_text = &text_orig[..text_orig.len() - text.len()];
    if kind == Name {
      for (i, symbol) in
        keywords.0.split_ascii_whitespace().enumerate()
      {
        if token_text == symbol {
          kind = keywords.1[i];
          break;
        }
      }
    }
    result.push(Token { kind, text: token_text.to_string() })
  }
  return result;

  fn name_char(c: char) -> bool {
    matches!(c, '_' | 'a'..='z' | 'A'..='Z' | '0'..='9')
  }

  fn trim(
    text: &str,
    predicate: impl std::ops::Fn(char) -> bool,
  ) -> Option<&str> {
    let index =
      text.find(|it: char| !predicate(it)).unwrap_or(text.len());
    if index == 0 {
      None
    } else {
      Some(&text[index..])
    }
  }
}

#[derive(Debug)]
enum Event {
  Open { kind: TreeKind },
  Close,
  Advance,
}

struct MarkOpened {
  index: usize,
}

struct MarkClosed {
  index: usize,
}

struct Parser {
  tokens: Vec<Token>,
  pos: usize,
  fuel: Cell<u32>,
  events: Vec<Event>,
}

impl Parser {
  fn new(tokens: Vec<Token>) -> Parser {
    Parser {
      tokens,
      pos: 0,
      fuel: Cell::new(256),
      events: Vec::new(),
    }
  }

  fn build_tree(self) -> Tree {
    let mut tokens = self.tokens.into_iter();
    let mut events = self.events;

    assert!(matches!(events.pop(), Some(Event::Close)));
    let mut stack = Vec::new();
    for event in events {
      match event {
        Event::Open { kind } => {
          stack.push(Tree { kind, children: Vec::new() })
        }
        Event::Close => {
          let tree = stack.pop().unwrap();
          stack
            .last_mut()
            .unwrap()
            .children
            .push(Child::Tree(tree));
        }
        Event::Advance => {
          let token = tokens.next().unwrap();
          stack
            .last_mut()
            .unwrap()
            .children
            .push(Child::Token(token));
        }
      }
    }

    let tree = stack.pop().unwrap();
    assert!(stack.is_empty());
    assert!(tokens.next().is_none());
    tree
  }

  fn open(&mut self) -> MarkOpened {
    let mark = MarkOpened { index: self.events.len() };
    self.events.push(Event::Open { kind: TreeKind::ErrorTree });
    mark
  }

  fn open_before(&mut self, m: MarkClosed) -> MarkOpened {
    let mark = MarkOpened { index: m.index };
    self.events.insert(
      m.index,
      Event::Open { kind: TreeKind::ErrorTree },
    );
    mark
  }

  fn close(
    &mut self,
    m: MarkOpened,
    kind: TreeKind,
  ) -> MarkClosed {
    self.events[m.index] = Event::Open { kind };
    self.events.push(Event::Close);
    MarkClosed { index: m.index }
  }

  fn advance(&mut self) {
    assert!(!self.eof());
    self.fuel.set(256);
    self.events.push(Event::Advance);
    self.pos += 1;
  }

  fn advance_with_error(&mut self, error: &str) {
    let m = self.open();
    // TODO: Error reporting.
    eprintln!("{error}");
    self.advance();
    self.close(m, ErrorTree);
  }

  fn eof(&self) -> bool {
    self.pos == self.tokens.len()
  }

  fn nth(&self, lookahead: usize) -> TokenKind {
    if self.fuel.get() == 0 {
      panic!("parser is stuck")
    }
    self.fuel.set(self.fuel.get() - 1);
    self
      .tokens
      .get(self.pos + lookahead)
      .map_or(TokenKind::Eof, |it| it.kind)
  }

  fn at(&self, kind: TokenKind) -> bool {
    self.nth(0) == kind
  }

  fn at_any(&self, kinds: &[TokenKind]) -> bool {
    kinds.contains(&self.nth(0))
  }

  fn eat(&mut self, kind: TokenKind) -> bool {
    if self.at(kind) {
      self.advance();
      true
    } else {
      false
    }
  }

  fn expect(&mut self, kind: TokenKind) {
    if self.eat(kind) {
      return;
    }
    // TODO: Error reporting.
    eprintln!("expected {kind:?}");
  }
}

use std::{cell::Cell, fmt};

use TokenKind::*;
use TreeKind::*;

fn file(p: &mut Parser) {
  let m = p.open();
  while !p.eof() {
    if p.at(FnKeyword) {
      func(p)
    } else {
      p.advance_with_error("expected a function");
    }
  }
  p.close(m, File);
}

fn func(p: &mut Parser) {
  assert!(p.at(FnKeyword));
  let m = p.open();
  p.expect(FnKeyword);
  p.expect(Name);
  if p.at(LParen) {
    param_list(p);
  }
  if p.eat(Arrow) {
    type_expr(p);
  }
  if p.at(LCurly) {
    block(p);
  }
  p.close(m, Fn);
}

const PARAM_LIST_RECOVERY: &[TokenKind] = &[FnKeyword, LCurly];
fn param_list(p: &mut Parser) {
  assert!(p.at(LParen));
  let m = p.open();

  p.expect(LParen);
  while !p.at(RParen) && !p.eof() {
    if p.at(Name) {
      param(p);
    } else {
      if p.at_any(PARAM_LIST_RECOVERY) {
        break;
      }
      p.advance_with_error("expected parameter");
    }
  }
  p.expect(RParen);

  p.close(m, ParamList);
}

fn param(p: &mut Parser) {
  assert!(p.at(Name));
  let m = p.open();

  p.expect(Name);
  p.expect(Colon);
  type_expr(p);
  if !p.at(RParen) {
    p.expect(Comma);
  }

  p.close(m, Param);
}

fn type_expr(p: &mut Parser) {
  let m = p.open();
  p.expect(Name);
  p.close(m, TypeExpr);
}

const STMT_RECOVERY: &[TokenKind] = &[FnKeyword];
const EXPR_FIRST: &[TokenKind] =
  &[Int, TrueKeyword, FalseKeyword, Name, LParen];
fn block(p: &mut Parser) {
  assert!(p.at(LCurly));
  let m = p.open();

  p.expect(LCurly);
  while !p.at(RCurly) && !p.eof() {
    match p.nth(0) {
      LetKeyword => stmt_let(p),
      ReturnKeyword => stmt_return(p),
      _ => {
        if p.at_any(EXPR_FIRST) {
          stmt_expr(p)
        } else {
          if p.at_any(STMT_RECOVERY) {
            break;
          }
          p.advance_with_error("expected statement");
        }
      }
    }
  }
  p.expect(RCurly);

  p.close(m, Block);
}

fn stmt_let(p: &mut Parser) {
  assert!(p.at(LetKeyword));
  let m = p.open();

  p.expect(LetKeyword);
  p.expect(Name);
  p.expect(Eq);
  expr(p);
  p.expect(Semi);

  p.close(m, StmtLet);
}

fn stmt_return(p: &mut Parser) {
  assert!(p.at(ReturnKeyword));
  let m = p.open();

  p.expect(ReturnKeyword);
  expr(p);
  p.expect(Semi);

  p.close(m, StmtReturn);
}

fn stmt_expr(p: &mut Parser) {
  let m = p.open();

  expr(p);
  p.expect(Semi);

  p.close(m, StmtExpr);
}

fn expr(p: &mut Parser) {
  expr_rec(p, Eof);
}

fn expr_rec(p: &mut Parser, left: TokenKind) {
  let Some(mut lhs) = expr_delimited(p) else {
    return;
  };

  while p.at(LParen) {
    let m = p.open_before(lhs);
    arg_list(p);
    lhs = p.close(m, ExprCall);
  }

  loop {
    let right = p.nth(0);
    if right_binds_tighter(left, right) {
      let m = p.open_before(lhs);
      p.advance();
      expr_rec(p, right);
      lhs = p.close(m, ExprBinary);
    } else {
      break;
    }
  }
}

fn right_binds_tighter(
  left: TokenKind,
  right: TokenKind,
) -> bool {
  fn tightness(kind: TokenKind) -> Option<usize> {
    [
      // Precedence table:
      [Plus, Minus].as_slice(),
      &[Star, Slash],
    ]
    .iter()
    .position(|level| level.contains(&kind))
  }
  let Some(right_tightness) = tightness(right) else {
    return false
  };
  let Some(left_tightness) = tightness(left) else {
    assert!(left == Eof);
    return true;
  };
  right_tightness > left_tightness
}

fn expr_delimited(p: &mut Parser) -> Option<MarkClosed> {
  let result = match p.nth(0) {
    TrueKeyword | FalseKeyword | Int => {
      let m = p.open();
      p.advance();
      p.close(m, ExprLiteral)
    }
    Name => {
      let m = p.open();
      p.advance();
      p.close(m, ExprName)
    }
    LParen => {
      let m = p.open();
      p.expect(LParen);
      expr(p);
      p.expect(RParen);
      p.close(m, ExprParen)
    }
    _ => return None,
  };
  Some(result)
}

fn arg_list(p: &mut Parser) {
  assert!(p.at(LParen));
  let m = p.open();

  p.expect(LParen);
  while !p.at(RParen) && !p.eof() {
    if p.at_any(EXPR_FIRST) {
      arg(p);
    } else {
      break;
    }
  }
  p.expect(RParen);

  p.close(m, ArgList);
}

fn arg(p: &mut Parser) {
  let m = p.open();
  expr(p);
  if !p.at(RParen) {
    p.expect(Comma);
  }
  p.close(m, Arg);
}

#[test]
fn smoke() {
  let text = "
fn f() {
  let x = 1 +
  let y = 2
}
";
  let cst = parse(text);
  eprintln!("{cst:?}");
}
