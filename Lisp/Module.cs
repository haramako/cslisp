using System;
using System.Collections.Generic;
using System.Text;

namespace Lisp
{
	public class Module
	{
		public readonly string Name;
		public readonly Dictionary<Symbol, Value> Bind = new Dictionary<Symbol, Value>();
		public HashSet<Symbol> Exports = new HashSet<Symbol>();

		public Module(string name)
		{
			Name = name;
		}

		public void Export(Symbol symbol)
		{
			Exports.Add(symbol);
		}
	}

}
