using System;
using System.Collections.Generic;

namespace Lisp
{
	public class Identifier
	{
		public Env Env;
		public Symbol Symbol;
		public Value Renamer;
	}
}
