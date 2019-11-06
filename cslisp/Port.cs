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
        char unreadBuf_;

        public string Filename { get; private set; }
        public int Line { get; private set; } = 1;

        public Port(Stream s, string filename = null)
        {
            s_ = s;
            r_ = new StreamReader(s_);
            Filename = filename;
        }

        public char ReadChar()
        {
            if (unreadBuf_ != '\0')
            {
                var c = unreadBuf_;
                unreadBuf_ = '\0';
                return c;
            }
            else
            {
                var c = (char)r_.Read();
                if( c == '\n')
                {
                    Line += 1;
                }
                return c;
            }
        }

        public void UnreadChar(char c)
        {
            if (unreadBuf_ != '\0')
            {
                throw new LuaException("Cant unread, already unread.");
            }
            else
            {
                unreadBuf_ = c;
            }
        }

    }
}
