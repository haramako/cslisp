using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Lisp
{
	public class Module
	{
		public readonly string Name;
		public readonly Dictionary<Symbol, Value> Bindings = new Dictionary<Symbol, Value>();
		public HashSet<Symbol> Exports = new HashSet<Symbol>();

		public Module(string name)
		{
			Name = name;
		}

		public void Export(Symbol symbol)
		{
			Exports.Add(symbol);
		}

		public void ExportFromEnv(Env env)
		{
			foreach (var symbol in Exports)
			{
				Value val;
				if (env.TryGet(symbol, out val))
				{
					Bindings[symbol] = val;
				}
				else
				{
					throw new LispException($"Not exported {symbol}");
				}
			}
		}

		public void ImportToEnv(Env env)
		{
			foreach (var key in Exports)
			{
				env.Define(key, Bindings[key]);
			}
		}

		public void ImportToEnv(Env env, ImportSet importSet)
		{
			foreach( var kv in importSet.Imports)
			{
				env.Define(kv.Value, Bindings[kv.Key]);
			}
		}

		public static string GetModuleName(Value symbolList)
		{
			return string.Join('.', Value.ListToArray(symbolList).Select(x => x.AsSymbol.ToString()));
		}
	}

}
