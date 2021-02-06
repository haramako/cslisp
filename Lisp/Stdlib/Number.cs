using System;
using System.Collections.Generic;
using System.Text;

namespace Lisp.Stdlib
{
	public static class Number
	{
		[LispApi("+")]
		public static Value add(Context ctx, Value[] args)
		{
			int r = 0;
			for( int i = 0; i < args.Length; i++)
			{
				r += args[i].AsInt;
			}
			return new Value(r);
		}

		[LispApi("-")]
		public static Value sub(Context ctx, Value[] args)
		{
			int r = args[0].AsInt;
			for (int i = 1; i < args.Length; i++)
			{
				r -= args[i].AsInt;
			}
			return new Value(r);
		}

		[LispApi("*")]
		public static Value mul(Context ctx, Value[] args)
		{
			int r = 1;
			for (int i = 0; i < args.Length; i++)
			{
				r *= args[i].AsInt;
			}
			return new Value(r);
		}

		[LispApi("/")]
		public static Value div(Context ctx, Value[] args)
		{
			int r = args[0].AsInt;
			for (int i = 1; i < args.Length; i++)
			{
				r /= args[i].AsInt;
			}
			return new Value(r);
		}

		[LispApi]
		public static Value truncate_quotient(Context ctx, Value v1, Value v2)
		{
			return new Value(v1.AsInt / v2.AsInt);
		}

		[LispApi]
		public static Value truncate_reminder(Context ctx, Value v1, Value v2)
		{
			return new Value(v1.AsInt % v2.AsInt);
		}

		[LispApi]
		public static Value floor_remainder(Context ctx, Value v1, Value v2)
		{
			return new Value(v1.AsInt % v2.AsInt);
		}

		[LispApi]
		public static Value floor(Context ctx, Value v)
		{
			return new Value(Math.Floor(v.ConvertToFloat()));
		}

		[LispApi]
		public static Value ceiling(Context ctx, Value v)
		{
			return new Value(Math.Ceiling(v.ConvertToFloat()));
		}

		[LispApi]
		public static Value round(Context ctx, Value v)
		{
			return new Value(Math.Round(v.ConvertToFloat()));
		}

		[LispApi]
		public static Value truncate(Context ctx, Value v)
		{
			return new Value(Math.Truncate(v.ConvertToFloat()));
		}

		[LispApi("=")]
		public static Value eq(Context ctx, Value[] args)
		{
			if (args.Length <= 0) return Value.T;
			int n = args[0].AsInt;
			for (int i = 1; i < args.Length; i++)
			{
				if (args[i].AsInt != n) return Value.F;
			}
			return Value.T;
		}


		[LispApi("<")]
		public static Value less(Context ctx, Value[] args)
		{
			if (args.Length <= 0) return Value.T;
			int n = args[0].AsInt;
			for (int i = 1; i < args.Length; i++)
			{
				var cur = args[i].AsInt;
				if (!(n < cur)) return Value.F;
				n = cur;
			}
			return Value.T;
		}

		[LispApi("<=")]
		public static Value less_eq(Context ctx, Value[] args)
		{
			if (args.Length <= 0) return Value.T;
			int n = args[0].AsInt;
			for (int i = 1; i < args.Length; i++)
			{
				var cur = args[i].AsInt;
				if (!(n <= cur)) return Value.F;
				n = cur;
			}
			return Value.T;
		}

		[LispApi(">")]
		public static Value greater(Context ctx, Value[] args)
		{
			if (args.Length <= 0) return Value.T;
			int n = args[0].AsInt;
			for (int i = 1; i < args.Length; i++)
			{
				var cur = args[i].AsInt;
				if (!(n > cur)) return Value.F;
				n = cur;
			}
			return Value.T;
		}

		[LispApi(">=")]
		public static Value greater_eq(Context ctx, Value[] args)
		{
			if (args.Length <= 0) return Value.T;
			int n = args[0].AsInt;
			for (int i = 1; i < args.Length; i++)
			{
				var cur = args[i].AsInt;
				if (!(n >= cur)) return Value.F;
				n = cur;
			}
			return Value.T;
		}

		[LispApi]
		public static Value abs(Context ctx, Value v)
		{
			return new Value(Math.Abs(v.AsInt));
		}

		[LispApi]
		public static Value exact(Context ctx, Value v)
		{
			return new Value(Math.Abs(v.ConvertToInt()));
		}

		[LispApi]
		public static Value inexact(Context ctx, Value v)
		{
			return new Value(Math.Abs(v.ConvertToFloat()));
		}

		[LispApi("exact?")]
		public static Value exact_p(Context ctx, Value v)
		{
			return new Value(v.IsInteger);
		}

		[LispApi("exact-integer?")]
		public static Value exact_integer_p(Context ctx, Value v)
		{
			return new Value(v.IsInteger);
		}

		[LispApi("inexact?")]
		public static Value inexact_p(Context ctx, Value v)
		{
			return new Value(v.IsFloat);
		}

		[LispApi]
		public static Value exact_integer_sqrt(Context ctx, Value v)
		{
			return new Value((int)Math.Sqrt(v.AsInt));
		}

		[LispApi("real?")]
		public static Value real_p(Context ctx, Value v)
		{
			return new Value(v.IsFloat);
		}
	}
}
