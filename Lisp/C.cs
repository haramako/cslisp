using System;
using System.Collections.Generic;
using System.Text;

namespace Lisp
{
	public static class C
	{
		public static readonly Value Nil = Value.Nil;

		public static readonly Value Begin = Value.Intern("begin");
		public static readonly Value QuasiQuote = Value.Intern("quasiquote");
		public static readonly Value Quote = Value.Intern("quote");
		public static readonly Value Unquote = Value.Intern("unquote");
		public static readonly Value UnquoteSplicing = Value.Intern("unquote-splicing");

		public static readonly Value Dot = Value.Intern(".");
		public static readonly Value Eof = Value.Intern("<eof>");
	}

}
