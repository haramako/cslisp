using System;
using System.Collections.Generic;
using System.Text;

namespace Lisp.Stdlib
{
	static class CharLib
	{
		[LispApi("char->integer")]
		public static Value char_to_integer(Context ctx, Value v)
		{
			return new Value((int)v.AsChar);
		}

		[LispApi("char=?")]
		public static Value char_eq(Context ctx, Value[] args)
		{
			if (args.Length <= 0) return Value.T;
			char n = args[0].AsChar;
			for (int i = 1; i < args.Length; i++)
			{
				if (args[i].AsChar != n) return Value.F;
			}
			return Value.T;
		}

		[LispApi("char<?")]
		public static Value char_less(Context ctx, Value[] args)
		{
			if (args.Length <= 0) return Value.T;
			char n = args[0].AsChar;
			for (int i = 1; i < args.Length; i++)
			{
				var cur = args[i].AsChar;
				if (!(n < cur)) return Value.F;
				n = cur;
			}
			return Value.T;
		}

		[LispApi("char<=?")]
		public static Value char_less_eq(Context ctx, Value[] args)
		{
			if (args.Length <= 0) return Value.T;
			char n = args[0].AsChar;
			for (int i = 1; i < args.Length; i++)
			{
				var cur = args[i].AsChar;
				if (!(n <= cur)) return Value.F;
				n = cur;
			}
			return Value.T;
		}

		[LispApi("char>?")]
		public static Value char_greater(Context ctx, Value[] args)
		{
			if (args.Length <= 0) return Value.T;
			char n = args[0].AsChar;
			for (int i = 1; i < args.Length; i++)
			{
				var cur = args[i].AsChar;
				if (!(n > cur)) return Value.F;
				n = cur;
			}
			return Value.T;
		}

		[LispApi("char>=?")]
		public static Value char_greater_eq(Context ctx, Value[] args)
		{
			if (args.Length <= 0) return Value.T;
			char n = args[0].AsChar;
			for (int i = 1; i < args.Length; i++)
			{
				var cur = args[i].AsChar;
				if (!(n >= cur)) return Value.F;
				n = cur;
			}
			return Value.T;
		}
	}
}
