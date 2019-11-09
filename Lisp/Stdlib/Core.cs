using System;
using System.Collections.Generic;
using System.Text;

namespace Lisp.Stdlib
{
	class Core
	{
		public static void Setup(Vm vm)
		{
			vm.RootEnv.Define(Symbol.Intern("display"), new Value(Display));
			vm.RootEnv.Define(Symbol.Intern("puts"), new Value(Puts));
			vm.RootEnv.Define(Symbol.Intern("begin"), new Value(Begin));
			vm.RootEnv.Define(Symbol.Intern("+"), new Value(Add));
		}

		public static Value Display(params Value[] args)
		{
			for (int i = 0; i < args.Length; i++)
			{
				Console.Write(args[i]);
				if (i < args.Length - 1)
				{
					Console.Write(" ");
				}
			}
			return Value.Nil;
		}

		public static Value Puts(params Value[] args)
		{
			Display(args);
			Console.WriteLine();
			return Value.Nil;
		}

		public static Value Begin(params Value[] args)
		{
			return args[args.Length - 1];
		}

		public static Value Add(params Value[] args)
		{
			var r = new Value(0);
			for (int i = 0; i < args.Length; i++)
			{
				r.AsInt = r.AsInt + args[i].AsInt;
			}
			return r;
		}

	}
}
