using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Lisp.Stdlib
{
	public static class StringLib
	{
		[LispApi("list->string")]
		public static Value list_to_string(Context ctx, Value v)
		{
			var sb = new StringBuilder();
			foreach (var c in Value.ListToArray(v))
			{
				sb.Append(c.AsChar);
			}
			return new Value(sb.ToString());
		}

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

		[LispApi("number->string")]
		public static Value number_to_string(Context ctx, Value v)
		{
			return new Value(v.AsInt.ToString());
		}

		[LispApi("utf8->string")]
		public static Value utf8_to_string(Context ctx, Value v)
		{
			return new Value(Encoding.UTF8.GetString(v.AsByteVector));
		}

		[LispApi]
		public static Value make_string(Context ctx, Value len, Value c)
		{
			return new Value(new string((char)c.AsInt, len.AsInt));
		}

		[LispApi]
		public static Value string_ref(Context ctx, Value s, Value idx)
		{
			return new Value(s.AsString[s.AsInt]);
		}

		[LispApi]
		public static Value string_length(Context ctx, Value s, Value idx)
		{
			return new Value(s.AsString.Length);
		}

		[LispApi("string=?")]
		public static Value string_eq(Context ctx, Value v1, Value v2)
		{
			return new Value(v1.AsString == v2.AsString);
		}


		[LispApi("string<?")]
		public static Value string_less(Context ctx, Value v1, Value v2)
		{
			return new Value(v1.AsString.CompareTo(v2.AsString) < 0);
		}

		[LispApi("string<=?")]
		public static Value string_less_eq(Context ctx, Value v1, Value v2)
		{
			return new Value(v1.AsString.CompareTo(v2.AsString) <= 0);
		}

		[LispApi("string>?")]
		public static Value string_greater(Context ctx, Value v1, Value v2)
		{
			return new Value(v1.AsString.CompareTo(v2.AsString) > 0);
		}

		[LispApi("string>=?")]
		public static Value string_greater_eq(Context ctx, Value v1, Value v2)
		{
			return new Value(v1.AsString.CompareTo(v2.AsString) >= 0);
		}

		[LispApi]
		public static Value substring(Context ctx, Value v, Value start, Value end)
		{
			return new Value(v.AsString.Substring(start.AsInt, end.AsInt - start.AsInt));
		}

		[LispApi]
		public static Value string_copy(Context ctx, Value[] args)
		{
			var v = args[0].AsString;
			int start = 0;
			int end;
			if ( args.Length >= 2)
			{
				start = args[1].AsInt;
			}
			if( args.Length >= 3)
			{
				end = args[2].AsInt;
			}
			else
			{
				end = v.Length;
			}
			return new Value(v.Substring(start, end - start));
		}

		[LispApi("string->number")]
		public static Value string_to_number(Context ctx, Value v)
		{
			return new Value(int.Parse(v.AsString));
		}

		[LispApi("string->list")]
		public static Value string_to_list(Context ctx, Value v)
		{
			return Value.ArrayToList(v.AsString.ToCharArray().Select(c => new Value(c)).ToArray());
		}

		[LispApi("string->utf8")]
		public static Value string_to_utf8(Context ctx, Value v)
		{
			var bytes = Encoding.UTF8.GetBytes(v.AsString);
			return new Value(bytes);
		}

	}
}
