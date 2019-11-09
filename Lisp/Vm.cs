using System;
using System.IO;
using System.Diagnostics;
using System.Collections.Generic;
using System.Text;
using System.Runtime.CompilerServices;

namespace Lisp
{
	public class LispApiAttribute : Attribute
	{
		public string Name { get; private set; }

		public LispApiAttribute(string name = null)
		{
			Name = name;
		}
	}

	public class LispException : Exception
	{
        public LispException() : base() { }
        public LispException(string msg) : base(msg) {}
	}

	public sealed class Closure
	{
		public Lambda Lambda;
		public Env Env;
		public bool IsSyntax;
		public Closure(Lambda lambda, Env env)
		{
			Lambda = lambda;
			Env = env;
		}
	}

	public class Context
	{

	}

	public class LispApi
	{
		public delegate Value FuncVararg(Context ctx, params Value[] param);
		public delegate Value Func0(Context ctx);
		public delegate Value Func1(Context ctx, Value v1);
		public delegate Value Func2(Context ctx, Value v1, Value v2);
		public delegate Value Func3(Context ctx, Value v1, Value v2, Value v3);
		public delegate Value Func4(Context ctx, Value v1, Value v2, Value v3, Value v4);
		public delegate Value Func5(Context ctx, Value v1, Value v2, Value v3, Value v4, Value v5);

		public Delegate Func;
		public int Arity;

		public LispApi(Delegate func)
		{
			Func = func;

			if( func is FuncVararg)
			{
				Arity = -1;
			}
			else if( func is Func0)
			{
				Arity = 0;
			}
			else if (func is Func1)
			{
				Arity = 1;
			}
			else if (func is Func2)
			{
				Arity = 2;
			}
			else if (func is Func3)
			{
				Arity = 3;
			}
			else if (func is Func4)
			{
				Arity = 4;
			}
			else if (func is Func5)
			{
				Arity = 5;
			}
			else
			{
				throw new ArgumentException($"Invalid func type {func}");
			}
		}
	}

	public class Vm
	{
		Compiler compiler_;
		Parser parser_ = new Parser();
		Eval eval_ = new Eval();
		Env rootEnv_ = new Env(null);

		public Env RootEnv => rootEnv_;

		public Vm()
		{
			ImportApi(typeof(Stdlib.Core));
			ImportApi(typeof(Stdlib.List));
			ImportApi(typeof(Stdlib.Number));
			ImportApi(typeof(Stdlib.Symbol));
			RootEnv.Define(Symbol.Intern("%if"), C.Nil);
			RootEnv.Define(Symbol.Intern("%define-syntax"), C.Nil);
			RootEnv.Define(Symbol.Intern("%define"), C.Nil);
			RootEnv.Define(Symbol.Intern("%lambda"), C.Nil);
			RootEnv.Define(Symbol.Intern("%quote"), C.Nil);
			RootEnv.Define(Symbol.Intern("%set!"), C.Nil);
			RootEnv.Define(Symbol.Intern("%begin"), C.Nil);
			RootEnv.Define(Symbol.Intern("begin"), C.Nil);
			RootEnv.Define(Symbol.Intern("quasiquote"), C.Nil);
			RootEnv.Define(Symbol.Intern("quote"), C.Nil);
			RootEnv.Define(Symbol.Intern("unquote"), C.Nil);
			RootEnv.Define(Symbol.Intern("unquote-splicing"), C.Nil);
			compiler_ = new Compiler(this);
		}

		#if false
		public Closure Compile(string src, string filename = null)
		{
			var s = new MemoryStream(Encoding.UTF8.GetBytes(src));
			return Compile(new Port(s, filename));
		}

		public Closure Compile(Port port)
		{
			var list = parser_.Parse(port);
			var lmd = compiler_.Compile(list);
			return new Closure(lmd, rootEnv_);
		}
		#endif

		public Value Run(string src, string filename = null)
		{
			var s = new MemoryStream(Encoding.UTF8.GetBytes(src));
			return Run(new Port(s, filename));
		}

		public Value Run(Port port)
		{
			Value result = C.Nil;
			while (true)
			{
				var list = parser_.Parse(port);
				if( list.IsNil)
				{
					return result;
				}
				var lmd = compiler_.Compile(list);
				var closure = new Closure(lmd, rootEnv_);
				result = Run(closure);
			}
		}

		public Value Run(Closure closure)
		{
			return eval_.Run(closure);
		}

		public Value Apply(Closure closure, params Value[] args)
		{
			return eval_.Apply(closure, args);
		}

		public void ImportApi(Type module)
		{
			foreach (var method in module.GetMethods())
			{
				var attributes = method.GetCustomAttributes(typeof(LispApiAttribute), true);
				foreach (var attr in attributes)
				{
					var api = (LispApiAttribute)attr;
					var name = api.Name;
					if( name == null)
					{
						name = method.Name.Replace('_', '-');
					}
					var param = method.GetParameters();
					Delegate del;
					switch (param.Length)
					{
						case 1:
							del = method.CreateDelegate(typeof(LispApi.Func0));
							break;
						case 2:
							if (param[1].ParameterType == typeof(Value))
							{
								del = method.CreateDelegate(typeof(LispApi.Func1));
							}
							else
							{
								del = method.CreateDelegate(typeof(LispApi.FuncVararg));
							}
							break;
						case 3:
							del = method.CreateDelegate(typeof(LispApi.Func2));
							break;
						case 4:
							del = method.CreateDelegate(typeof(LispApi.Func3));
							break;
						case 5:
							del = method.CreateDelegate(typeof(LispApi.Func4));
							break;
						case 6:
							del = method.CreateDelegate(typeof(LispApi.Func5));
							break;
						default:
							throw new LispException("Invalid parameter size");
					}
					var func = new LispApi(del);
					rootEnv_.Define(Symbol.Intern(name), new Value(func));
				}
			}
		}
	}
}
