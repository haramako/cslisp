using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace Lisp
{
	public class Port
	{
		Stream s_;
		StreamReader r_;
		int unreadBuf_;

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

		public Port(Stream s, string filename = "<unknown>")
		{
			s_ = s;
			r_ = new StreamReader(s_);
			Filename = filename;
		}

		public int ReadChar()
		{
			if (unreadBuf_ != '\0')
			{
				var c = unreadBuf_;
				unreadBuf_ = '\0';
				return c;
			}
			else
			{
				var c = r_.Read();
				if( c == '\n')
				{
					Line += 1;
				}
				return c;
			}
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

		public Value ReadValue()
		{
			if( parser_ == null)
			{
				parser_ = new Parser();
			}

			return parser_.Parse(this);
		}

	}
}
