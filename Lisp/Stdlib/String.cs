using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Lisp.Stdlib
{
	public static class StringLib
	{
		[LispApi("string->symbol")]
		public static Value string_to_symbol(Context ctx, Value v)
		{
			return Value.Intern(v.AsString);
		}

		[LispApi]
		public static Value string_append(Context ctx, Value[] args)
		{
			return new Value(string.Join("", args.Select(s => s.AsString)));
		}

		[LispApi("%number->string")]
		public static Value number_to_string(Context ctx, Value v)
		{
			return new Value(v.AsInt.ToString());
		}

		[LispApi]
		public static Value make_string(Context ctx, Value len, Value c)
		{
			return new Value(new string((char)c.AsInt, len.AsInt));
		}
	}
}
