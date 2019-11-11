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

		public static Value Intern(string symbol) => new Value(Symbol.Intern(symbol));

		public static readonly Value T = new Value(true);
		public static readonly Value F = new Value(false);

		//====================================================
		// Cons
		//====================================================

		public Value Car => this.AsCons.Car;
		public Value Cdr => this.AsCons.Cdr;

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

		//====================================================
		// List
		//====================================================

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

		static Value appendReverse(Value a, Value b)
		{
			var tail = a;
			for( var cur = b; !cur.IsNil; cur = cur.Cdr)
			{
				tail = Cons(b.Car, tail);
			}
			return tail;
		}

		public static Value Append(Value[] list)
		{
			if (list.Length <= 0) return C.Nil;
			if (list.Length <= 1) return list[0];

			var head = Value.Cons(C.Nil, C.Nil);
			var tail = head;
			for( int i = 0; i < list.Length - 1; i++)
			{
				var curList = list[i];
				for (var cur = curList; !cur.IsNil; cur = cur.Cdr)
				{
					var newTail = Value.Cons(cur.Car, C.Nil);
					tail.AsCons.Cdr = newTail;
					tail = newTail;
				}
			}

			tail.AsCons.Cdr = list[list.Length - 1];

			return head.Cdr;
		}

		public static Value Reverse(Value list)
		{
			return appendReverse(C.Nil, list);
		}

		//====================================================
		// Equality
		//====================================================

		public static bool Eq(Value a, Value b)
		{
			if (a.val_ == b.val_)
			{
				switch (a.ValueType)
				{
					case ValueType.String:
					case ValueType.Table:
					case ValueType.LispApi:
					case ValueType.Object:
					case ValueType.Symbol:
					case ValueType.Cons:
						return a.obj_ == b.obj_;
					default:
						return true;
				}
			}
			else
			{
				if (a.IsNumber)
				{
					switch (a.ValueType)
					{
						case ValueType.Integer:
							if (b.IsFloat)
							{
								return b.AsFloat == a.ConvertToFloat();
							}
							else
							{
								return false;
							}
						case ValueType.Float:
							if (b.IsInteger)
							{
								return b.ConvertToFloat() == a.AsFloat;
							}
							else
							{
								return false;
							}
						default:
							return false;
					}
				}
				else
				{
					return false;
				}
			}
		}

		public static bool Eqv(Value a, Value b)
		{
			return a == b;
		}

		public static List<Value> deepEqualCache = new List<Value>();

		public static bool DeepEqual(Value a, Value b)
		{
			var r = deepEqualSub(a, b);
			deepEqualCache.Clear();
			return r;
		}

		public static bool deepEqualSub(Value a, Value b)
		{
			if (a == b)
			{
				return true;
			}
			else
			{
				var vt = a.ValueType;
				if (vt == b.ValueType)
				{
					switch (vt)
					{
						case ValueType.Cons:
							if (a.obj_ == b.obj_)
							{
								return true;
							}
							else
							{
								return deepEqualSub(a.Car, b.Car) && deepEqualSub(a.Cdr, b.Cdr);
							}
						default:
							return false;
					}
				}
				else
				{
					return false;
				}
			}
		}
	}
}
