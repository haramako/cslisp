using System;
using System.Collections.Generic;
using System.Text;

namespace Lisp.Stdlib
{
	public static class Symbol
	{
		[LispApi("symbol->string")]
		public static Value symbol_to_string(Context ctx, Value v)
		{
			return new Value(v.AsSymbol.ToString());
		}

		[LispApi]
		public static Value gensym(Context ctx)
		{
			throw new Exception();
			#if false
			return gensym();
			#endif
		}

	}
}
