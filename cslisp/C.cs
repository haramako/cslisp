using System;
using System.Collections.Generic;
using System.Text;

namespace Lisp
{
    internal static class C
    {
        public static readonly Value Nil = Value.Nil;
        public static readonly Value QuasiQuote = new Value("quasiquote");
        public static readonly Value Quote = new Value("quote");
        public static readonly Value Unquote = new Value("unquote");
        public static readonly Value UnquoteSplicing = new Value("unquote-splicing");

        public static readonly Value Dot = new Value(Symbol.Intern("."));
    }

}
