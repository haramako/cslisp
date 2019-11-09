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

		public static Value WithLocation(Port src, Value car, Value cdr)
		{
			var cons = new Cons(car, cdr);
			cons.SrcFilename = src.Filename;
			cons.SrcLine = src.Line;
			return new Value(cons);
		}

		public static Value WithLocation(Value src, Value car, Value cdr)
		{
			var cons = new Cons(car, cdr);
			var srcCons = src.AsCons;
			cons.SrcFilename = srcCons.SrcFilename;
			cons.SrcLine = srcCons.SrcLine;
			return new Value(cons);
		}

		public SourceLocation Location => new SourceLocation { Filename = SrcFilename, Line = SrcLine };
	}

	public static class ConsUtil
	{
		public static Value Cons(Value v1, Value v2)
		{
			return Value.Cons(v1, v2);
		}

		public static Value Cons(Value v1, Value v2, Value v3)
		{
			return Value.Cons(v1, Value.Cons(v2, v3));
		}

		public static Value Cons(Value v1, Value v2, Value v3, Value v4)
		{
			return Value.Cons(v1, Value.Cons(v2, Value.Cons(v3, v4)));
		}

		public static Value Cons(Value v1, Value v2, Value v3, Value v4, Value v5)
		{
			return Value.Cons(v1, Value.Cons(v2, Value.Cons(v3, Value.Cons(v4, v5))));
		}

		public static void Bind2(Value src, out Value a, out Value b)
		{
			var cur = src;
			a = cur.Car;
			cur = cur.Cdr;
			b = cur.Car;
		}

		public static void Bind2Rest(Value src, out Value a, out Value b, out Value rest)
		{
			var cur = src;
			a = cur.Car;
			cur = cur.Cdr;
			b = cur.Car;
			rest = cur.Cdr;
		}

		public static void Bind3(Value src, out Value a, out Value b, out Value c)
		{
			var cur = src;
			a = cur.Car;
			cur = cur.Cdr;
			b = cur.Car;
			cur = cur.Cdr;
			c = cur.Car;
		}

		public static void Bind3Rest(Value src, out Value a, out Value b, out Value c, out Value rest)
		{
			var cur = src;
			a = cur.Car;
			cur = cur.Cdr;
			b = cur.Car;
			cur = cur.Cdr;
			c = cur.Car;
			rest = cur.Cdr;
		}


		public static Value ReverseInplace(Value list)
		{
			if( list.IsNil)
			{
				return list;
			}

			var tail = C.Nil;
			var cur = list;
			while (!cur.IsNil)
			{
				var next = cur.Cdr;
				cur.AsCons.Cdr = tail;
				tail = cur;
				cur = next;
			}
			return tail;
		}
	}

}
