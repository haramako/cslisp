﻿using System;
using System.Collections.Generic;
using System.Text;

namespace Lisp.Stdlib
{
	public static class Symbol
	{
		[LispApi]
		public static Value symbol_to_string(Context ctx, Value v) /* 1 */
		{
			return new Value(v.AsSymbol.ToString());
		}

		[LispApi(">=")]
		public static Value gensym(Context ctx) /* 0 */
		{
			throw new Exception();
			#if false
			return gensym();
			#endif
		}

	}
}