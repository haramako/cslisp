using System;
using System.Collections.Generic;
using System.Text;

namespace Lisp
{
	public class Cons
	{
		public Value Car;
		public Value Cdr;

		public string SrcFilename
		{
			get;
			private set;
		}
		public int SrcLine
		{
			get;
			private set;
		}

		public Cons(Value car, Value cdr)
		{
			Car = car;
			Cdr = cdr;
		}

		public static Value WithLocation(string filename, int line, Value car, Value cdr)
		{
			var cons = new Cons(car, cdr);
			cons.SrcFilename = filename;
			cons.SrcLine = line;
			return new Value(cons);
		}

		public static Value WithLocation(Port src, Value car, Value cdr)
		{
			return WithLocation(src.Filename, src.Line, car, cdr);
		}

		public static Value WithLocation(Value src, Value car, Value cdr)
		{
			var srcCons = src.AsCons;
			return WithLocation(srcCons.SrcFilename, srcCons.SrcLine, car, cdr);
		}

		public SourceLocation Location => new SourceLocation { Filename = SrcFilename, Line = SrcLine };
	}

}
