using System;
using System.IO;
using System.Text;

namespace Lisp
{
	public struct UpvalTag
	{
		public byte InStack;
		public byte Index;
	}

	public sealed class Function
	{
		public string Filename;
		public string Name;
		public int LineStart;
		public int LineEnd;
		public int ParamNum;
		public bool HasVarArg;
		public int MaxStackSize;

		public uint[] Codes;
		public Value[] Consts;
		public UpvalTag[] Upvals;
		public Function[] Protos;
		public uint[] DebugInfos;

		// Root用のダミー関数
		public Function()
		{
			Upvals = new UpvalTag[0];
		}
	}
}
