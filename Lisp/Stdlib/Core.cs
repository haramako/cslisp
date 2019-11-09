using System;
using System.Collections.Generic;
using System.Text;

namespace Lisp.Stdlib
{
	class Core
	{
		public static void Setup(Vm vm)
		{
			vm.RootEnv.Define(Symbol.Intern("display"), new Value(display));
			vm.RootEnv.Define(Symbol.Intern("puts"), new Value(puts));
			vm.RootEnv.Define(Symbol.Intern("begin"), new Value(begin));
			vm.RootEnv.Define(Symbol.Intern("+"), new Value(add));
		}

		[LispApi]
		public static Value display(params Value[] args)
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

		[LispApi]
		public static Value puts(params Value[] args)
		{
			display(args);
			Console.WriteLine();
			return Value.Nil;
		}

		[LispApi]
		public static Value begin(params Value[] args)
		{
			return args[args.Length - 1];
		}

		[LispApi("+")]
		public static Value add(params Value[] args)
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
