using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace Lisp
{
	public class Port : IDisposable
	{
		Stream s_;
		StreamReader r_;
		StreamWriter w_;
		int unreadBuf_;
		bool disposed_;

		Parser parser_;


		public string Filename
		{
			get;
			private set;
		}
		public int Line
		{
			get;
			private set;
		} = 1;

		public readonly bool IsBinary;

		public Port(Stream s, string filename = "<unknown>", bool isBinary = false)
		{
			s_ = s;
			if (s.CanRead)
			{
				r_ = new StreamReader(s_);
			}
			if (s_.CanWrite)
			{
				w_ = new StreamWriter(s_);
			}
			Filename = filename;
			IsBinary = false;
		}

		int readBuf()
		{
			if (unreadBuf_ != '\0')
			{
				var c = unreadBuf_;
				unreadBuf_ = '\0';
				return c;
			}
			else
			{
				return -1;
			}
		}

		public int PeekChar()
		{
			int c = ReadChar();
			UnreadChar(c);
			return c;
		}

		public int ReadChar()
		{
			int c = readBuf();
			if (c == -1)
			{
				c = r_.Read();
				if (c == '\n')
				{
					Line += 1;
				}
			}
			return c;
		}

		public void UnreadChar(int c)
		{
			if (unreadBuf_ != '\0')
			{
				throw new LispException("Cant unread, already unread.");
			}
			else
			{
				unreadBuf_ = c;
			}
		}

		public bool IsReady
		{
			get
			{
				if (unreadBuf_ != '\0')
				{
					return true;
				}
				else
				{
					return !r_.EndOfStream;
				}
			}
		}

		public Value ReadValue()
		{
			if( parser_ == null)
			{
				parser_ = new Parser();
			}

			return parser_.Parse(this);
		}

		public void WriteChar(char c)
		{
			w_.Write(c);
		}

		public void Dispose()
		{
			if (!disposed_)
			{
				r_.Close();
				disposed_ = true;
			}
		}

		public void Flush()
		{
			s_.Flush();
			if( w_ != null)
			{
				w_.Flush();
			}
		}

		public bool IsOpen => !disposed_;
		public SourceLocation Location => new SourceLocation(Filename, Line);
		public bool IsInputPort => s_.CanRead;
		public bool IsOutputPort => s_.CanWrite;
		public Stream RawStream => s_;

	}
}
