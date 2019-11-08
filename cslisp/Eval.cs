using System;
using System.Collections.Generic;
using System.Text;

namespace Lisp
{
	public class Env
	{
		Dictionary<Symbol, Value> dict_ = new Dictionary<Symbol, Value>();
		Env up_;

		public Env(Env up)
		{
			up_ = up;
		}

		public Env Up => up_;

		public Value Get(Symbol symbol)
		{
			Value found;
			if (dict_.TryGetValue(symbol, out found))
			{
				return found;
			}
			else if (up_ != null)
			{
				return up_.Get(symbol);
			}
			else
			{
				throw new Exception($"Symbol '{symbol}' not found");
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
				throw new LuaException($"{symbol} not defined");
			}
		}
	}

	public struct Dump
	{
		public Closure Closure;
		public int Pc;

		public Dump(Closure closure, int pc)
		{
			Closure = closure;
			Pc = pc;
		}
	}

	public class Eval
	{
		int ii;

		public Eval(int i)
		{
			ii = i;
		}

		static T[] popMulti<T>(Stack<T> stack, int n)
		{
			var r = new T[n];
			for (int i = n - 1; i >= 0; i--)
			{
				r[i] = stack.Pop();
			}
			return r;
		}

		public Value Run(Closure closure)
		{
			int pc = 0;
			Code[] codes = closure.Lambda.Code;

			var s = new Stack<Value>();
			Env e = closure.Env;
			Code c;
			var d = new Stack<Dump>();

			while (true)
			{
				c = codes[pc++];
				Console.WriteLine(c);
				switch( c.Op)
				{
					case Operator.Ldc:
						s.Push(c.Val);
						break;
					case Operator.Ld:
						{
							var sym = c.Val.AsSymbol;
							var val = e.Get(sym);
							s.Push(val);
						}
						break;
					case Operator.Def:
						{
							var val = s.Peek();
							var sym = c.Val.AsSymbol;
							e.Define(sym, val);
						}
						break;
					case Operator.Set:
						{
							var val = s.Pop();
							var sym = s.Pop().AsSymbol;
							e.Set(sym, val);
						}
						break;
					case Operator.Syntax:
						{
							var val = s.Pop();
							var sym = s.Pop().AsSymbol;
							e.Define(sym, val);
						}
						break;
					case Operator.Ldf:
						{
							s.Push(new Value(new Closure(c.Val.As<Lambda>(), e)));
						}
						break;
					case Operator.Goto:
						{
							pc = c.Val.AsInt;
						}
						break;
					case Operator.Ret:
						{
							Dump restored;
							if( d.TryPop( out restored))
							{
								closure = restored.Closure;
								pc = restored.Pc;
								e = closure.Env;
							}
							else
							{
								return s.Pop();
							}
						}
						break;
					case Operator.If:
						{
							var val = s.Pop();
							if (val.IsNil || !val.AsBool)
							{
								pc = c.Val.AsInt;
							}
						}
						break;
					case Operator.Ap:
						{
							var len = c.Val.AsInt;
							var args = popMulti(s, len - 1);
							var applicant = s.Pop();
							var vt = applicant.ValueType;
							if ( vt == ValueType.Closure)
							{
								var cl = applicant.AsClosure;
								d.Push(new Dump());
								e = cl.Env;
								codes = cl.Lambda.Code;
								pc = 0;
							}
							else if( vt == ValueType.LuaApi)
							{
								var func = applicant.AsLuaApi;
								s.Push(func.Invoke(args));
							}
							else
							{
								throw new Exception($"Can't apply {applicant}");
							}
						}
						break;
					default:
						throw new Exception("BUG");
				}
			}
		}

	}
}
