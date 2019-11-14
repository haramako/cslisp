using System;
using System.IO;
using System.Text;
using System.Diagnostics;
using Lisp;
using Lisp.Debugging;

class Program
{
	static TextWriter dumpWriter;
	static bool verbose = false;
	static Vm vm;

	static void Main(string[] args)
	{
		vm = new Vm();

		try
		{
			for (int i = 0; i < args.Length; i++)
			{
				var arg = args[i];
				switch (arg)
				{
					case "-v": // verbose
						verbose = true;
						break;
					case "--dump": // compile only
						{
							dumpWriter = new StreamWriter(File.OpenWrite(args[i + 1]));
							i++;
						}
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
							var port = new Port(new MemoryStream(File.ReadAllBytes(arg)), arg);
							run(port);
						}
						break;
				}
			}
		}
		catch (ExitException ex)
		{
			if (verbose)
			{
				vm.PrintStatistics();
			}
			Environment.Exit(ex.Code);
		}
		catch (Exception ex)
		{
			Console.WriteLine(ex);
			throw;
			//waitKey();
		}
		finally
		{
			if (dumpWriter != null)
			{
				foreach (var lmd in vm.Compiler.Lambdas)
				{
					new CodePrinter().PrintLambda(dumpWriter, lmd);
				}
				dumpWriter.Dispose();
			}
		}
		waitKey();
	}

	static void run(Port port)
	{
		var result = vm.Run(port);
		Console.WriteLine(result);
		if (verbose)
		{
			vm.PrintStatistics();
		}
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
