using System;
using System.Collections.Generic;

namespace Lisp
{
	public class Identifier
	{
		static int CurrentId;

		public Env Env;
		public Symbol Symbol;
		public Value Renamer;
		public int Id;

		public Identifier()
		{
			Id = CurrentId++;
		}
	}
}
