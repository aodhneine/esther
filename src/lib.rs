//! Root crate for the Esther automated proof assistant.

#![forbid(missing_docs)]

/* def double (a : nat) : nat := a + a */

/* type nat :=
| zero : nat
| succ (n : nat) : nat */

/* theorem id (p : prop) : p → p */

/// Representation of a token in a source code. Unlike in other languages, we
/// don't have specialised token types, which allows us to easily add custom
/// syntax definitions right in the source code.
#[derive(Copy, Clone)]
pub struct Token<'a> {
	// Pair of offsets into a string containing the token. We don't expect to be
	// handling files with more than 2^32 bytes (4,294,967,296 bytes!) which also
	// allows us to save few bytes of space.
	start: u32,
	end: u32,
	// Represents a source code that this token is a view into. This means that
	// you cannot drop a source code while still using tokens that you got from
	// it.
	_source: core::marker::PhantomData<&'a str>,
}

// We need custom Debug implementation for Token to avoid exposing its internal
// fields (such as `_source`.)
impl core::fmt::Debug for Token<'_> {
	fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
		return f.debug_struct("Token")
			.field("start", &self.start)
			.field("end", &self.end)
			.finish();
	}
}

impl<'a> Token<'a> {
	/// Creates new token from `start` to `end`.
	pub fn new(start: u32, end: u32) -> Self {
		return Self {
			start,
			end,
			_source: core::marker::PhantomData,
		};
	}
}

// TODO: Replace keeping an offset and string slice with Chars iterator, which
// can do all that for us, for free.

/// Represents a single source text.
pub struct Source<'a> {
	text: &'a str,
	offset: usize,
}

macro_rules! debug {
	($msg : expr) => {
		std::eprintln!(core::concat!("\x1b[2m[dbg]\x1b[0m ", $msg))
	};

	($msg : expr, $($e : expr),*) => {
		std::eprintln!(core::concat!("\x1b[2m[dbg]\x1b[0m ", $msg), $($e),*)
	};
}

mod unicode {
	/// Returns either the next rune (UTF-8 code point), or None, if there is no
	/// characters left in `s`.
	#[inline]
	pub fn get_rune(s: &str, p: usize) -> Option<char> {
		// SAFETY: We assume that p is a valid offset into a string. It is up to the
		// caller to uphold that assumption.
		return unsafe {
			s.get_unchecked(p..)
		}.chars().next();
	}
}

impl<'a> Source<'a> {
	/// Creates new representation of the source text.
	pub fn new(text: &'a str) -> Self {
		return Self {
			text,
			offset: 0,
		};
	}

	// TODO: Refactor this whole character handling mess we made... Probably by
	// taking an approach similar to rustc_lexer, by only storing .chars iterator
	// and working directly with it.

	// We use \0 to mark end-of-file, which makes working witch characters easier
	// but it doesn't mark the TRUE end of file, as it is returned in a case that
	// Chars iterator fails to get the next character. This can happen either
	// when we hit the end of file, or when the next character is not a proper
	// UTF-8 character. This means that you can get EOF even though there are
	// still bytes in the file. If you need to find the true end of file, you can
	// use `is_eof` function.
	const EOF: char = '\0';

	fn get_char(&self) -> char {
		return unicode::get_rune(self.text, self.offset).unwrap_or(Self::EOF);
	}

	fn peek_char(&self) -> char {
		let i = match unicode::get_rune(self.text, self.offset) {
			Some(c) => c.len_utf8(),
			// If there is no runes left, we return EOF char.
			None => return Self::EOF,
		};

		return unicode::get_rune(self.text, self.offset + i).unwrap();
	}

	/* #[inline]
	const fn is_eof(&self) -> bool {
		return self.text.len() == self.offset;
	} */

	// Returns true if the given character is a “terminator”, i.e. a whitespace
	// or a special punctuation character.
	fn is_terminator(c: char) -> bool {
		return c.is_whitespace() || c == '.' || c == '(' || c == ')' || c == ':';
	}

	// TODO: Make Source.next return an error that describes what exactly went
	// wrong. Maybe we could do something like Lean parser does — that would
	// require using parser combinators instead.

	// We want to support scientific notation, rational numbers (i.e. 1/3), and a
	// lot of custom operators. We preferably want to split source code by delim.
	// and whitespace.

	// We need to ignore normal comments (both line and block), but preserve the
	// doc comments.

	// TODO: This is STILL REALLY BAD and I don't like it the least. But it works
	// so I guess I can not care about it for now... We definitely should come up
	// with a better solution.

	/// Returns a next token parsed from the source text, or none if there is no
	/// characters left, or there is an invalid character in the stream.
	pub fn next(&mut self) -> Option<Token<'a>> {
		loop {
			match self.get_char() {
				'-' => match self.peek_char() {
					// Parse (and ignore) line comments.
					'-' => {
						self.offset += 2;

						loop {
							match self.get_char() {
								'\n' | Self::EOF => break,
								c => self.offset += c.len_utf8(),
							};
						};

						// Since comment is not a token, and we haven't parsed a token yet,
						// we need to continue looping to hope for a token (or the end of
						// file.)
						continue;
					},
					_ => todo!("‘-’ + everything else"),
				},
				'/' => match self.peek_char() {
					// Parse (and ignore) block comments (except when they are doc
					// comments, in which case we need to preserve them.)
					'-' => todo!("block comment"),
					_ => todo!("‘/’ + everything else"),
				},
				// Skip all whitespace.
				c if c.is_whitespace() => self.offset += c.len_utf8(),
				// Terminate the loop on EOF char (‘\0’).
				Self::EOF => return None,
				// Identifiers.
				c if c.is_alphabetic() => {
					let start = self.offset;
					self.offset += c.len_utf8();

					loop {
						match self.get_char() {
							c if Self::is_terminator(c) => break,
							Self::EOF => break,
							c => self.offset += c.len_utf8(),
						};
					};

					return Some(Token::new(start as u32, self.offset as u32));
				},
				c => {
					let start = self.offset;
					self.offset += c.len_utf8();
					return Some(Token::new(start as u32, self.offset as u32));
				},
			};
		};
	}

	/// Returns the underlying text for the given token.
	#[inline]
	pub fn at_token(&self, token: Token<'a>) -> &'a str {
		// SAFETY: Each token contains information about the associated lifetime,
		// as such it's impossible to create a token which refers to the given
		// source text, but represents an invalid offset, because the only place
		// where we can construct such token is Source.next.
		return unsafe {
			self.text.get_unchecked(token.start as usize..token.end as usize)
		};
	}
}

/// Syntax tree representation of the Esther source code.
pub mod syntax {
	#[derive(Debug)]
	pub enum Node<'a> {
		/// Empty node, used as a sentinel element.
		None,
		/// Identifier.
		Ident(crate::Token<'a>),
	}

	#[derive(Debug)]
	pub struct NodeId(usize);

	impl NodeId {
		pub fn new(id: usize) -> Self {
			return Self(id);
		}
	}

	pub struct Ident<'a> {
		token: crate::Token<'a>,
	}

	pub struct Type<'a> {
		ty: crate::Token<'a>,
	}

	/* pub struct Def<'a> {
		ident: Ident<'a>,
		return_type: Type<'a>,
		body: &'a [...],
	} */
}

/// Parser for the Esther syntax.
pub mod parse {
	/// Parser context.
	pub struct Parser<'a> {
		tokens: &'a [crate::Token<'a>],
		source: &'a crate::Source<'a>,
		tree: std::vec::Vec<crate::syntax::Node<'a>>,
		tk_offset: usize,
	}

	impl<'a> Parser<'a> {
		pub fn new(tokens: &'a [crate::Token<'a>], source: &'a crate::Source<'a>) -> Self {
			return Self {
				tokens,
				source,
				tree: std::vec::Vec::new(),
				tk_offset: 0,
			};
		}

		fn pop_token(&mut self) -> Option<crate::Token<'a>> {
			if self.tokens.len() == self.tk_offset {
				return None;
			}

			let token = unsafe {
				self.tokens.get_unchecked(self.tk_offset)
			};

			self.tk_offset += 1;
			return Some(*token);
		}

		fn new_node(&mut self, node: crate::syntax::Node<'a>) -> crate::syntax::NodeId {
			let id = self.tree.len();
			self.tree.push(node);
			return crate::syntax::NodeId::new(id);
		}

		fn expect_token_str(&mut self, token: &'a str) -> Option<crate::Token<'a>> {
			let maybe_expected_token = self.pop_token()?;
			let maybe_expected_token_str = self.source.at_token(maybe_expected_token);

			if maybe_expected_token_str != token {
				eprintln!("\x1b[1;31merror:\x1b[0m expected ‘{}’, found ‘{}’", token, maybe_expected_token_str);
				return None;
			}

			return Some(maybe_expected_token);
		}

		/// Try to parse a ‘def’ declaration from the token stream. Returns None on
		/// failure.
		pub fn parse_def(&mut self) -> Option<crate::syntax::NodeId> {
			let def_kwd = self.expect_token_str("def")?;
			let start = def_kwd.start;

			// Parse def. name.
			let ident = /* self.parse_ident */ self.pop_token()?;
			let ident_id = self.new_node(crate::syntax::Node::Ident(ident));

			// Parse argument list.
			self.expect_token_str("(")?;
			/* self.parse_arg_list */
			self.expect_token_str(")")?;

			// Parse return type.
			self.expect_token_str(":")?;
			/* self.parse_type */

			// Parse assignment statement.
			self.expect_token_str(":")?;
			self.expect_token_str("=")?;

			// Parse def. body.
			/* self.parse_block */

			return None;
		}
	}

	impl core::fmt::Debug for Parser<'_> {
		fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
			return f.debug_struct("Parser")
				.field("tree", &self.tree)
				.finish();
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn it_works() {
		// debug!("size_of(Token) = {:?}", core::mem::size_of::<Token<'_>>());

		let source = std::fs::read_to_string("tests/1.est").expect("failed to open file");
		let mut source = Source::new(&source);
		let mut tokens = Vec::new();

		loop {
			match source.next() {
				Some(token) => tokens.push(token),
				None => break,
			};
		};

		let mut parser = parse::Parser::new(&tokens, &source);
		debug!("{:?}", parser.parse_def());

		// Check the final parser state. We can now get the syntax tree from it.
		debug!("{:?}", parser);
	}
}
