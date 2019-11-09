using System;
using System.Collections.Generic;
using System.Text;

namespace Lisp.Stdlib
{
	class Core
	{
		[LispApi]
		public static Value display(Context ctx, params Value[] args)
		{
			for (int i = 0; i < args.Length; i++)
			{
				Console.Write(args[i]);
				if (i < args.Length - 1)
				{
					Console.Write(" ");
				}
			}
			return Value.Nil;
		}

		[LispApi]
		public static Value puts(Context ctx, params Value[] args)
		{
			display(ctx, args);
			Console.WriteLine();
			return Value.Nil;
		}

		[LispApi("+")]
		public static Value add(Context ctx, params Value[] args)
		{
			var r = new Value(0);
			for (int i = 0; i < args.Length; i++)
			{
				r.AsInt = r.AsInt + args[i].AsInt;
			}
			return r;
		}

		[LispApi]
		public static Value identity(Context ctx, Value v)
		{
			return v;
		}

		[LispApi("eq?")]
		public static Value eq_p(Context ctx, Value[] args)
		{
			if (args.Length <= 1) return Value.T;
			var x = args[0];
			for (int i = 1; i < args.Length; i++)
			{
				var v = args[i];
				if (v != x) return Value.F;
			}
			return Value.T;
		}

		[LispApi("eqv?")]
		public static Value eqv_p(Context ctx, Value[] args)
		{
			if (args.Length <= 1) return Value.T;
			var x = args[0];
			for (int i = 1; i < args.Length; i++)
			{
				var v = args[i];
				if (!Value.Eqv(v, x)) return Value.F;
			}
			return Value.T;
		}

		[LispApi("equal?")]
		public static Value equal_p(Context ctx, Value[] args)
		{
			if (args.Length <= 1) return Value.T;
			var x = args[0];
			for (int i = 1; i < args.Length; i++)
			{
				var v = args[i];
				if (!Value.Equal(v, x)) return Value.F;
			}
			return Value.T;
		}

		[LispApi("define?")]
		public static Value define_p(Context ctx, Value sym)
		{
			#if false
			ERROR_IF_NOT_SYMBOL(sym);
			return bundle_find(ctx->bundle, V2SYMBOL(sym), true, false) ? VALUE_T : VALUE_F;
			#endif
			return Value.F;
		}

		[LispApi("!")]
		public static Value not(Context ctx, Value v)
		{
			return new Value(v == Value.F);
		}

		[LispApi("number?")]
		public static Value number_p(Context ctx, Value v)
		{
			return new Value(v.IsInteger);
		}

		[LispApi("char?")]
		public static Value char_p(Context ctx, Value v)
		{
			return Value.F;
			#if false
			return new Value(v.IsChar);
			#endif
		}

		[LispApi("symbol?")]
		public static Value symbol_p(Context ctx, Value v)
		{
			return new Value(v.IsSymbol);
		}

		[LispApi("pair?")]
		public static Value pair_p(Context ctx, Value v)
		{
			return new Value(v.IsCons);
		}

		[LispApi("null?")]
		public static Value _null_p(Context ctx, Value v)
		{
			return new Value(v.IsNil);
		}

		[LispApi("list?")]
		public static Value _list_p(Context ctx, Value v)
		{
			return new Value(v.IsCons || v.IsNil);
		}

		[LispApi("string?")]
		public static Value _string_p(Context ctx, Value v)
		{
			return new Value(v.IsString);
		}

		[LispApi("procedure?")]
		public static Value procedure_p(Context ctx, Value v)
		{
			return new Value(v.IsClosure || v.IsLispApi);
		}

		[LispApi("macro?")]
		public static Value macro_p(Context ctx, Value v)
		{
			return new Value(v.IsClosure && v.AsClosure.IsSyntax);
		}

	}
}
