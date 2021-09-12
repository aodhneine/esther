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

impl core::convert::From<Token<'_>> for core::ops::Range<usize> {
	fn from(value: Token<'_>) -> Self {
		return Self {
			start: value.start as usize,
			end: value.end as usize,
		};
	}
}

// TODO: Replace keeping an offset and string slice with Chars iterator, which
// can do all that for us, for free.

/// Single source code without any other associated information.
pub struct Src<'a> {
	text: &'a str,
	offset: usize,
	eof: bool,
}

#[allow(unused_macros)]
macro_rules! debug {
	($msg : expr) => {
		std::eprintln!(core::concat!("\x1b[2m[dbg]\x1b[0m ", $msg))
	};

	($msg : expr, $($e : expr),*) => {
		std::eprintln!(core::concat!("\x1b[2m[dbg]\x1b[0m ", $msg), $($e),*)
	};
}

// Internal module with utilities for working with UTF-8 encoded strings.
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

impl<'a> Src<'a> {
	/// Creates new representation of the source text.
	pub fn new(text: &'a str) -> Self {
		return Self {
			text,
			offset: 0,
			eof: false,
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

	// TODO: Make Src.next return an error that describes what exactly went
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
				// Terminate the loop on EOF char (‘\0’) only if we returned an EOF
				// token before.
				Self::EOF if self.eof => return None,
				Self::EOF => {
					self.eof = true;
					return Some(Token::new(self.offset as u32, self.offset as u32));
				},
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
	pub fn at_token(&self, token: &Token<'a>) -> &'a str {
		// SAFETY: Each token contains information about the associated lifetime,
		// as such it's impossible to create a token which refers to the given
		// source text, but represents an invalid offset, because the only place
		// where we can construct such token is Src.next.
		return unsafe {
			self.text.get_unchecked(core::ops::Range::from(*token))
		};
	}

	/// Return a line, column pair for a given token.
	fn position_at_token(&self, token: &Token<'a>) -> (u32, u32) {
		// A Unicode newline. Because I don't like how b'\n' looks like in the source
		// code.
		const NEWLINE: u8 = 0xa;

		let mut line = 1;
		let mut column = 1;
		let mut iter = self.text
			.as_bytes()
			.iter()
			.cloned()
			.zip(0..);

		while let Some((c, i)) = iter.next() {
			if i == token.start {
				break;
			}

			column += 1;

			if c == NEWLINE {
				column = 1;
				line += 1;
				continue;
			}
		};

		return (line, column);
	}
}

// We want a separate struct to be able to keep information about a) the parser
// state, and b) the tokens we are working with.
/// A parsing context.
pub struct Parser<'a> {
	tokens: &'a [Token<'a>],
	source: &'a Src<'a>,
}

impl<'a> Parser<'a> {
	/// Constructs a new parser instance, [`Parser`].
	pub fn new(tokens: &'a [Token<'a>], source: &'a Src<'a>) -> Self {
		return Self {
			tokens,
			source,
		};
	}

	fn has_token(&self, expected: &str) -> bool {
		let token = unsafe {
			self.tokens.get_unchecked(0)
		};

		return self.source.at_token(token) == expected;
	}

	fn advance(&mut self) {
		self.tokens = unsafe {
			self.tokens.get_unchecked(1..)
		};
	}
}

impl Parser<'_> {
	// 'def' ident '(' arg_list ')' ':' type ':=' body
	fn parse_def(&mut self) {
		if self.has_token("def") {
			self.advance();
		}

		// Return an error...
	}
}


// @todo We need a proper test harness for the lexer and parser.

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn it_works() {
		debug!("size_of(Token) = {:?}", core::mem::size_of::<Token<'_>>());
		debug!("size_of(&Token) = {:?}", core::mem::size_of::<&Token<'_>>());

		let source = std::fs::read_to_string("tests/1.est").expect("failed to open file");
		let mut source = Src::new(&source);
		let mut tokens = Vec::new();

		loop {
			match source.next() {
				Some(token) => {
					let (line, column) = source.position_at_token(&token);
					debug!("line {}, column {}", line, column);
					// We need to translate the line we got into an offset into the iter.
					debug!("{:?}", source.text.lines().nth(line as usize - 1));

					tokens.push(token);
				},
				None => break,
			};
		};

		debug!("{:?}", tokens.iter().map(|token| source.at_token(token)).collect::<Vec<_>>());

		let mut parser = Parser::new(&tokens, &source);
		debug!("{:?}", parser.parse_def());
	}
}
