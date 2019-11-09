using System;
using System.Collections.Generic;
using System.Text;

namespace Lisp
{
	public class Parser
	{
		void skipSpace(Port s)
		{
			for (; ; )
			{
				int c = s.ReadChar();
				switch (c)
				{
					case '\n':
					case '\r':
					case '\x0c':
					case ' ':
					case '\t':
						break;
					case ';':
						for (; ; )
						{
							c = s.ReadChar();
							if (c == '\n' || c == '\r' || c == '\x0c' || c == '\0' || c == -1) break;
						}
						break;
					default:
						s.UnreadChar(c);
						return;
				}
			}
		}

		Value parseList(Port s)
		{
			skipSpace(s);
			int c;
			switch (c = s.ReadChar())
			{
				case ')':
				case ']':
				case '\0':
				case -1:
					s.UnreadChar(c);
					return C.Nil;
				default:
					s.UnreadChar(c);
					var val = Parse(s);

					if (val == C.Dot)
					{
						var cdr = Parse(s);
						skipSpace(s);
						return cdr;
					}
					else
					{
						var filename = s.Filename;
						var line = s.Line;
						var cdr = parseList(s);
						return Value.ConsSrc(filename, line, val, cdr);
					}
			}
		}

		static bool isSymbolChar(int c)
		{
			switch (c)
			{
				case ' ':
				case '\t':
				case '\n':
				case '\r':
				case '\x0c':
				case '(':
				case ')':
				case '[':
				case ']':
				case '\0':
				case -1:
					return false;
				default:
					return true;
			}
		}

		static string readToken(Port s)
		{
			var sb = new StringBuilder();
			int c;
			for (; ; )
			{
				c = s.ReadChar();
				if (!isSymbolChar(c)) break;
				sb.Append((char)c);
			}
			s.UnreadChar(c);
			return sb.ToString();
		}

		static bool isNumber(char c)
		{
			return (c >= '0' && c <= '9');
		}

		static Value parseToken(string str)
		{
			if (isNumber(str[0]) || (str.Length >= 2 && str[0] == '-' && isNumber(str[1])))
			{
				for (int i = 0; i < str.Length; i++)
				{
					if (!isNumber(str[i]))
					{
						return new Value(Symbol.Intern(str));
					}
				}
				return new Value(int.Parse(str));
			}
			else
			{
				return new Value(Symbol.Intern(str));
			}
		}

		char unescapeChar(Port s)
		{
			int c = s.ReadChar();
			//char buf[9];
			switch (c)
			{
				case '"':
					return '"';
				case '\\':
					return '\\';
				case 'n':
					return '\n';
				case 'r':
					return '\r';
				case 'f':
					return '\f';
				case 't':
					return '\t';
				case '0':
					return '\0';
					#if false
				case 'x':
					stream_read(s, buf, 2);
					buf[2] = '\0';
					sscanf(buf, "%x", &c);
					return c;
				case 'u':
					stream_read(s, buf, 4);
					buf[4] = '\0';
					sscanf(buf, "%x", &c);
					return c;
				case 'U':
					stream_read(s, buf, 8);
					buf[8] = '\0';
					sscanf(buf, "%x", &c);
					return c;
				case ' ':
				case '\n':
				case '\r':
					{
						bool feeded = false;
						for (; ; )
						{
							if (c == '\n')
							{
								if (feeded) assert(0);
								feeded = true;
							}
							else if (c == ' ' || c == '\f' || c == '\r' || c == '\t')
							{
								// skip
							}
							else
							{
								if (!feeded) assert(0);
								stream_ungetc(c, s);
								break;
							}
							c = stream_getc(s);
						}
						return '\n';
					}
					#endif
				default:
					throw new LispException($"unknown escaped string {c}\n");
			}
		}

		Value readString(Port s)
		{
			var sb = new StringBuilder();
			for (; ; )
			{
				int c = s.ReadChar();
				switch (c)
				{
					case '"':
						return new Value(sb.ToString());
					case '\\':
						c = unescapeChar(s);
						break;
					case '\n':
					case '\r':
						throw new Exception();
					case '\0':
					case -1:
						throw new Exception();
				}
				sb.Append((char)c);
			}
		}

		public Value Parse(Port s)
		{
			skipSpace(s);
			int c;
			switch (c = s.ReadChar())
			{
				case '\0':
				case -1:
					return C.Nil;
				case '(':
				case '[':
					{
						var result = parseList(s);
						var c2 = s.ReadChar();
						if (c == '(' && c2 != ')') throw new Exception();
						if (c == '[' && c2 != ']') throw new Exception();
						return result;
					}
				case ')':
				case ']':
					throw new LispException("paren not matched");
				case '#':
					switch (c = s.ReadChar())
					{
						case 't':
							return new Value(true);
						case 'f':
							return new Value(false);
						case ';':
							// s-exp comment
							Parse(s); // skip
							return Parse(s);
						default:
							throw new LispException($"invalid #char {c}\n");
					}
				case '"':
					return readString(s);
				case '\'':
					{
						var result = Parse(s);
						result = Value.ConsSrc(s, C.Quote, Value.ConsSrc(s, result, Value.Nil));
						return result;
					}
				case '`':
					{
						var result = Parse(s);
						result = Value.ConsSrc(s, C.QuasiQuote, Value.ConsSrc(s, result, Value.Nil));
						return result;
					}
				case ',':
					{
						Value sym = C.Unquote;
						if ((c = s.ReadChar()) == '@')
						{
							sym = C.UnquoteSplicing;
						}
						else
						{
							s.UnreadChar(c);
						}

						var result = Parse(s);
						result = Value.ConsSrc(s, sym, Value.ConsSrc(s, result, Value.Nil));
						return result;
					}
				default:
					{
						s.UnreadChar(c);
						var buf = readToken(s);
						return parseToken(buf);
					}
			}
			throw new Exception("Must not reach");
		}

		public Value ParseList(Port port)
		{
			var tail = C.Nil;
			while (true)
			{
				var code = Parse(port);
				if( code.IsNil)
				{
					return Value.ReverseInplace(tail);
				}
				tail = new Value(new Cons(code, tail));
			}
		}

	}
}
