using System;
using System.Collections.Generic;
using System.Text;

namespace Lisp
{
	public partial struct Value
	{
		//====================================================
		// Utility methods
		//====================================================

		public Value Car => this.AsCons.Car;
		public Value Cdr => this.AsCons.Cdr;

		public static Value Intern(string symbol) => new Value(Symbol.Intern(symbol));

		public static readonly Value T = new Value(true);
		public static readonly Value F = new Value(false);


		public static bool Eqv(Value a, Value b)
		{
			return a == b;
		}

		public static bool DeepEqual(Value a, Value b)
		{
			return a == b;
		}

		public static Value Cons(Value car, Value cdr)
		{
			return new Value(new Cons(car, cdr));
		}

		public static Value ConsSrc(Value src, Value car, Value cdr)
		{
			return new Value(new Cons(src.AsCons.Location, car, cdr));
		}

		public static Value ConsSrc(Port port, Value car, Value cdr)
		{
			return new Value(new Cons(port.Location, car, cdr));
		}

		public static Value ConsSrc(string filename, int line, Value car, Value cdr)
		{
			return new Value(new Cons(new SourceLocation(filename, line), car, cdr));
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
			if (list.IsNil)
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

		public static int ListLength(Value list)
		{
			int len = 0;
			for( var cur = list; !cur.IsNil; cur = cur.Cdr)
			{
				len++;
			}
			return len;
		}

		public static Value[] ListToArray(Value list)
		{
			var r = new Value[ListLength(list)];

			int i = 0;
			for (var cur = list; !cur.IsNil; cur = cur.Cdr)
			{
				r[i++] = cur.Car;
			}
			return r;
		}
	}
}
