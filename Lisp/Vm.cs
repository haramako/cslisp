﻿using System;
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

	public class LuaException : Exception
	{
		public LuaException(string msg) : base(msg)
		{
		}
	}

	public sealed class CallInfo
	{
		public int Func;
		public int Result;
		public int Base;
		public int SavedPc;
		public int Wanted;
		public CallInfo Prev;
	}

	public class Upval
	{
		public Value Val;
		public int Index;
		public bool IsOpen;
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

	public delegate Value LispApi(params Value[] param);

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
					var func = (LispApi)method.CreateDelegate(typeof(LispApi));
					rootEnv_.Define(Symbol.Intern(name), new Value(func));
				}
			}
		}
	}
}
