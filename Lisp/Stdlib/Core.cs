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


	}
}
