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
			if ( TryGet(symbol, out found))
			{
				return found;
			}
			else
			{
				throw new LuaException($"Symbol '{symbol}' not found");
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
		Closure closure_;

		Stack<Value> stack_ = new Stack<Value>(); // SECDマシンの"S"
		Env env_; // SECDマシンの"E"
		Code[] code_; // SECDマシンの"C"
		int pc_; // SECDマシンの"C"
		Stack<Dump> dump_ = new Stack<Dump>(); // SECDマシンの"D"

		public Eval()
		{
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


		public Value Apply(Closure closure, params Value[] args)
		{
			closure_ = closure;
			var lmd = closure.Lambda;

			env_ = closure.Env;
			for (int i = 0; i < args.Length; i++)
			{
				env_.Define(lmd.Params[i], args[i]);
			}

			code_ = closure.Lambda.Code;
			pc_ = 0;

			return Execute();
		}

		public Value Run(Closure closure)
		{
			closure_ = closure;
			code_ = closure.Lambda.Code;
			env_ = closure.Env;
			return Execute();
		}

		public Value Execute()
		{
			int pc = 0;
			Code[] code = code_;

			var closure = closure_;
			var s = stack_;
			Env e = env_;
			var d = dump_;

			Code c;
			while (true)
			{
				var location = closure.Lambda.Locations[pc];
				c = code[pc++];
				//Console.WriteLine("{0} at {1}:{2}", c, location.Filename, location.Line);
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
					case Operator.Pop:
						{
							s.Pop();
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
							var val = s.Peek();
							var sym = c.Val.AsSymbol;
							e.Set(sym, val);
						}
						break;
					case Operator.Syntax:
						{
							var val = s.Peek();
							var sym = c.Val.AsSymbol;
							val.AsClosure.IsSyntax = true;
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
								code = closure.Lambda.Code;
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
								d.Push(new Dump(closure, pc));
								var cl = applicant.AsClosure;
								var lmd = cl.Lambda;
								e = cl.Env;
								code = cl.Lambda.Code;
								closure = cl;

								for ( int i = 0; i < args.Length; i++)
								{
									e.Define(lmd.Params[i], args[i]);
								}
								pc = 0;
							}
							else if( vt == ValueType.LispApi)
							{
								var func = applicant.AsLispApi;
								Value result;
								Context ctx = null;
								switch( func.Arity)
								{
									case 0:
										result = ((LispApi.Func0)func.Func)(ctx);
										break;
									case 1:
										result = ((LispApi.Func1)func.Func)(ctx, args[0]);
										break;
									case 2:
										result = ((LispApi.Func2)func.Func)(ctx, args[0], args[1]);
										break;
									case 3:
										result = ((LispApi.Func3)func.Func)(ctx, args[0], args[1], args[2]);
										break;
									case 4:
										result = ((LispApi.Func4)func.Func)(ctx, args[0], args[1], args[2], args[3]);
										break;
									case 5:
										result = ((LispApi.Func5)func.Func)(ctx, args[0], args[1], args[2], args[3], args[4]);
										break;
									default:
										result = ((LispApi.Flex)func.Func)(ctx, args);
										break;
								}
								s.Push(result);
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
