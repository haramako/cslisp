using System;
using System.IO;
using System.Diagnostics;
using System.Collections.Generic;
using System.Text;
using System.Runtime.CompilerServices;
using System.Linq;

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
		public SourceLocation Location;

		public LispException() : base() { }
		public LispException(string msg) : base(msg) {}

		public void SetLocation(SourceLocation location)
		{
			Location = location;
		}
	}

	public class ExitException : LispException
	{
		public readonly int Code;
		public ExitException(int code) : base("Exit program")
		{
			Code = code;
		}
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

		public override string ToString()
		{
			return Lambda.ToString();
		}
	}

	public class Context
	{
		public readonly Vm Vm;
		public Env Env;

		public Context(Vm vm)
		{
			Vm = vm;
		}
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

		public readonly Delegate Func;
		public readonly int Arity;
		public readonly Symbol Name;

		public LispApi(Delegate func, Symbol name)
		{
			Func = func;
			Name = name;

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
		//Compiler compiler_;
		Parser parser_ = new Parser();
		Port currentPort_;
		Eval eval_;
		Env defaultEnv_ = new Env(null);
		//Env rootEnv_ = new Env(null);
		Dictionary<string, Module> modules_ = new Dictionary<string, Module>();

		//public Compiler Compiler => compiler_;
		public Parser Parser => parser_;
		public Eval Eval => eval_;
		//public Env RootEnv => rootEnv_;
		public Dictionary<string, Module> Modules => modules_;
		public List<Lambda> Lambdas = new List<Lambda>();

		public Vm(bool noPrelude = false)
		{
			eval_ = new Eval(this);
			provideEmbedModules();
			if (noPrelude)
			{
				Modules["%embeded"].ImportToEnv(defaultEnv_);
			}
			else
			{
				provideBaseModules();
			}
		}

		public Port CurrentPort => currentPort_;

		public void provideEmbedModules()
		{
			Env e = new Env(null);

			ImportApi(typeof(Stdlib.Core), e);
			ImportApi(typeof(Stdlib.List), e);
			ImportApi(typeof(Stdlib.Number), e);
			ImportApi(typeof(Stdlib.Symbol), e);
			ImportApi(typeof(Stdlib.StringLib), e);
			ImportApi(typeof(Stdlib.Misc), e);
			ImportApi(typeof(Stdlib.BooleanLib), e);
			ImportApi(typeof(Stdlib.CharLib), e);

			e.Define(Symbol.Intern("%if"), C.Nil);
			e.Define(Symbol.Intern("%define-syntax"), C.Nil);
			e.Define(Symbol.Intern("%define"), C.Nil);
			e.Define(Symbol.Intern("%lambda"), C.Nil);
			e.Define(Symbol.Intern("%quote"), C.Nil);
			e.Define(Symbol.Intern("%set!"), C.Nil);
			e.Define(Symbol.Intern("%begin"), C.Nil);
			e.Define(Symbol.Intern("begin"), C.Nil);
			e.Define(Symbol.Intern("quasiquote"), C.Nil);
			e.Define(Symbol.Intern("quote"), C.Nil);
			e.Define(Symbol.Intern("unquote"), C.Nil);
			e.Define(Symbol.Intern("unquote-splicing"), C.Nil);
			e.Define(Symbol.Intern("apply"), C.Nil);
			e.Define(Symbol.Intern("%make-current-continuation"), C.Nil);
			e.Define(Symbol.Intern("define-library"), C.Nil);
			e.Define(Symbol.Intern("import"), C.Nil);

			var embedModule = new Module("%embeded");

			foreach (var sym in e.RawDict.Keys)
			{
				embedModule.Export(sym);
			}
			embedModule.ExportFromEnv(e);
			Modules[embedModule.Name] = embedModule;
		}


		public void provideBaseModules()
		{
			Env env = new Env(null);
			var filename = Path.GetFullPath("lib/prelude.scm");
			Run(File.ReadAllText("lib/prelude.scm"), filename, env);
		}

		public Value Run(string src, string filename = null, Env env = null)
		{
			env = env ?? defaultEnv_;
			var s = new MemoryStream(Encoding.UTF8.GetBytes(src));
			return Run(new Port(s, filename), env);
		}

		public Value Run(Port port, Env env = null)
		{
			env = env ?? defaultEnv_;
			var oldPort = currentPort_;
			currentPort_ = port;

			var compiler = new Compiler(this, env);
			Value result = C.Nil;
			while (true)
			{
				var list = parser_.Parse(port);
				if( list.IsNil)
				{
					currentPort_ = oldPort;
					return result;
				}
				var lmd = compiler.Compile(list);

				var closure = new Closure(lmd, env);
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

		public void ImportApi(Type module, Env env)
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
					var nameSymbol = Symbol.Intern(name);
					var func = new LispApi(del, nameSymbol);
					env.Define(nameSymbol, new Value(func));
				}
			}
		}

		public void PrintStatistics()
		{
			var s = eval_.Statistics;
			var opCount = s.ExecCount.Sum();

			Console.WriteLine("==== Statistics ====");
			Console.WriteLine("== General ==");
			Console.WriteLine("OpCount   : {0,8}", opCount);
			Console.WriteLine("MaxStack  : {0,8}", s.MaxStack);
			Console.WriteLine("MaxDump   : {0,8}", s.MaxDump);
			Console.WriteLine("ApLisp    : {0,8}", s.ApLispCount);
			Console.WriteLine("ApNative  : {0,8}", s.ApNativeCount);

			Console.WriteLine("== Operators ==");
			for (int i = 0; i < s.ExecCount.Length; i++)
			{
				Console.WriteLine("{0,-6}: {1,8} ({2:00.0}%)", (Operator)i, s.ExecCount[i], 100f * s.ExecCount[i] / opCount);
			}

			Console.WriteLine("== Call ==");
			foreach (var kv in s.ApplyCount.OrderBy(kv => kv.Value).Reverse().Take(30))
			{
				Console.WriteLine("{0,-20}: {1,8} ({2:00.0}%)", kv.Key, kv.Value, 100f * kv.Value / (s.ApLispCount + s.ApNativeCount));
			}
		}

	}
}
