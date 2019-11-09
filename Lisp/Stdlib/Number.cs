using System;
using System.Collections.Generic;
using System.Text;

namespace Lisp.Stdlib
{
	public static class Number
	{
		[LispApi("+")]
		public static Value add(Context ctx, Value[] args) /* -1 + */
		{
			int r = 0;
			for( int i = 0; i < args.Length; i++)
			{
				r += args[i].AsInt;
			}
			return new Value(r);
		}

		[LispApi("-")]
		public static Value sub(Context ctx, Value[] args) /* -1 - */
		{
			int r = args[0].AsInt;
			for (int i = 1; i < args.Length; i++)
			{
				r -= args[i].AsInt;
			}
			return new Value(r);
		}

		[LispApi("*")]
		public static Value mul(Context ctx, Value[] args) /* -1 * */
		{
			int r = 1;
			for (int i = 0; i < args.Length; i++)
			{
				r *= args[i].AsInt;
			}
			return new Value(r);
		}

		[LispApi("/")]
		public static Value div(Context ctx, Value[] args) /* -1 / */
		{
			int r = args[0].AsInt;
			for (int i = 1; i < args.Length; i++)
			{
				r /= args[i].AsInt;
			}
			return new Value(r);
		}

		[LispApi("%")]
		public static Value modulo(Context ctx, Value[] args) /* -1 */
		{
			int r = args[0].AsInt;
			for (int i = 1; i < args.Length; i++)
			{
				r %= args[i].AsInt;
			}
			return new Value(r);
		}

		[LispApi("=")]
		public static Value eq(Context ctx, Value[] args) /* -1 = */
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
				if (!(cur < n)) return Value.F;
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
				if (!(cur <= n)) return Value.F;
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
				if (!(cur > n)) return Value.F;
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
				if (!(cur >= n)) return Value.F;
				n = cur;
			}
			return Value.T;
		}
	}
}
