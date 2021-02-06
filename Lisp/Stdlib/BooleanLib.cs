using System;
using System.Collections.Generic;
using System.Text;

namespace Lisp.Stdlib
{
	class BooleanLib
	{
		[LispApi("boolean=?")]
		public static Value boolean_eq(Context ctx, Value[] args)
		{
			if (args.Length <= 0) return Value.T;
			bool n = args[0].AsBool;
			for (int i = 1; i < args.Length; i++)
			{
				if (args[i].AsBool != n) return Value.F;
			}
			return Value.T;
		}
	}
}
