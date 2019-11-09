using System;
using System.IO;
using System.Diagnostics;
using System.Collections.Generic;
using System.Text;
using System.Runtime.CompilerServices;

namespace Lisp
{
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
		public Closure(Lambda lambda, Env env)
		{
			Lambda = lambda;
			Env = env;
		}
	}

	public delegate Value LuaApi(params Value[] param);

	public class Vm
	{
		Compiler compiler_ = new Compiler();
		Parser parser_ = new Parser();
		Eval eval_ = new Eval();
		Env rootEnv_ = new Env(null);

		public Env RootEnv => rootEnv_;

		public Vm()
		{
			Stdlib.Core.Setup(this);
		}

		public Closure Compile(string src, string filename = null)
		{
			var s = new MemoryStream(Encoding.UTF8.GetBytes(src));
			return Compile(new Port(s, filename));
		}

		public Closure Compile(Port port)
		{
			var list = parser_.ParseList(port);
			var lmd = compiler_.CompileBlock(list);
			return new Closure(lmd, rootEnv_);
		}

		public Value Run(string src, string filename = null)
		{
			var s = new MemoryStream(Encoding.UTF8.GetBytes(src));
			return Run(new Port(s, filename));
		}

		public Value Run(Port port)
		{
			return Run(Compile(port));
		}

		public Value Run(Closure closure)
		{
			return eval_.Run(closure);
		}

	}
}
