using System;
using System.Collections.Generic;
using System.Text;

namespace Lisp
{
	public class Env
	{
		public readonly string ModuleName;

		Dictionary<Symbol, Value> dict_ = new Dictionary<Symbol, Value>();
		Env up_;


		public Dictionary<Symbol, Value> RawDict => dict_;

		public Env(Env up, string moduleName = null)
		{
			up_ = up;
			ModuleName = moduleName;
		}

		public Env Up => up_;

		public bool TryGet(Symbol symbol, out Value val)
		{
			Value found;
			if (dict_.TryGetValue(symbol, out found))
			{
				val = found;
				return true;
			}
			else if (up_ != null)
			{
				return up_.TryGet(symbol, out val);
			}
			else
			{
				val = C.Nil;
				return false;
			}
		}

		public Value Get(Symbol symbol)
		{
			Value found;
			if (TryGet(symbol, out found))
			{
				return found;
			}
			else
			{
				throw new LispException($"Symbol '{symbol}' not found");
			}
		}


		public void Define(Symbol symbol, Value val)
		{
			dict_[symbol] = val;
		}

		public void Set(Symbol symbol, Value val)
		{
			Value found;
			if (dict_.TryGetValue(symbol, out found))
			{
				dict_[symbol] = val;
			}
			else if (up_ != null)
			{
				up_.Set(symbol, val);
			}
			else
			{
				throw new LispException($"{symbol} not defined");
			}
		}
	}

}
