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

        public Port(Stream s)
        {
            s_ = s;
            r_ = new StreamReader(s_);
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
                return (char)r_.Read();
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
