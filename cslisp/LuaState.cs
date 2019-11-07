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
		public Function Func;
		public Upval[] Upvals;
		public Closure(Function func)
		{
			Func = func;
			Upvals = new Upval[Func.Upvals.Length];
		}
	}

	public delegate void LuaApi(LuaState L);

	public class LuaState
	{
	}
}
