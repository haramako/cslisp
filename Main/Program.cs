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
		#if !DEBUG
		try
		{
		#endif
			for (int i = 0; i < args.Length; i++)
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
			#if !DEBUG
		}
		catch (Exception ex)
		{
			//waitKey();
			throw;
		}
			#endif
		waitKey();
	}

	static void run(Port port)
	{
		var vm = new Vm();
		var result = vm.Run(port);
		Console.WriteLine(result);
		vm.PrintStatistics();
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

}
