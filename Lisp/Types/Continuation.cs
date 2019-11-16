using System;
using System.Collections.Generic;

namespace Lisp
{
	public class Continuation
	{
		public int Pc;
		public Code[] Code;
		public Closure Closure;
		public Stack<Value> Stack;
		public Env Env;
		public Stack<Dump> Dump;
	}
}
