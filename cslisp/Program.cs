using System;
using Lisp;
using System.IO;
using System.Text;
using System.Diagnostics;

class Program
{
	static bool compileOnly = false;
	static bool verbose = false;
	static void Main(string[] args)
	{
		for( int i = 0; i < args.Length; i++)
		{
			var arg = args[i];
			switch (arg)
			{
				case "-v": // verbose
					verbose = true;
					break;
				case "-c": // compile only
					compileOnly = true;
					break;
				case "-e": // execute string
					{
						var s = new MemoryStream(Encoding.UTF8.GetBytes(args[i + 1]));
						i++;
						var port = new Port(s, null);
						run(port);
					}
					break;
				default:
					{
						var port = new Port(File.OpenRead(arg), arg);
						run(port);
					}
					break;
			}
		}
		waitKey();
	}

	static void run(Port port)
	{

		var parser = new Parser();
		var list = parser.ParseList(port);
		list = new Value(new Cons(C.Begin, list));

		var compiler = new Compiler();
		var lmd = compiler.Compile(list);

		if (verbose)
		{
			for (int i = 0; i < lmd.Code.Length; i++)
			{
				Console.WriteLine("{0:0000}: {1}", i, lmd.Code[i]);
			}
		}

		if (compileOnly)
		{
			return;
		}

		var eval = new Eval(0);
		var env = new Env(null);
		env.Define(Symbol.Intern("begin"), new Value(begin));
		env.Define(Symbol.Intern("+"), new Value(add));
		var closure = new Closure(lmd, env);

		var result = eval.Run(closure);
		Console.WriteLine(result);

	}

	/// <summary>
	/// デバッガがアタッチされている場合は、キーの入力を待つ
	///
	/// Windowsの開津環境で、コンソールがすぐに閉じてしまうのの対策として使用している
	/// </summary>
	static void waitKey()
	{
		if (Debugger.IsAttached)
		{
			Console.ReadKey();
		}
	}

	static Value begin(params Value[] args)
	{
		return args[args.Length - 1];
	}

	static Value add(params Value[] args)
	{
		var r = new Value(0);
		for (int i = 0; i < args.Length; i++)
		{
			r.AsInt = r.AsInt + args[i].AsInt;
		}
		return r;
	}


}
