using NUnit.Framework;
using Lisp;
using System.IO;
using System;
using System.Text;

namespace Tests
{
	public class Tests
	{
		PrettyPrinter pp_ = new PrettyPrinter();

		[SetUp]
		public void Setup()
		{
		}

		// ユーティリティ

		public void pp(Value v)
		{
			Console.WriteLine(pp_.Print(v));
		}

		Value parse(string src)
		{
			var s = new MemoryStream(Encoding.UTF8.GetBytes(src));
			var port = new Port(s);
			var parser = new Parser();

			var result = parser.Parse(port);

			return result;
		}

		// テスト

		[TestCase("1", "1")]
		[TestCase(@"""hoge""", @"""hoge""")]
		[TestCase(@"symbol", @"symbol")]
		[TestCase(@"symbol-with-line", @"symbol-with-line")]
		[TestCase("#t", "#t")]
		[TestCase("#f", "#f")]
		[TestCase("()", "()")]
		[TestCase("(1)", "(1)")]
		[TestCase("(1 . 2)", "(1 . 2)")]
		[TestCase("(1 . (2 3))", "(1 2 3)")]
		[TestCase("(1 2 ( 3 4 ) )", "(1 2 (3 4))")]
		public void TestParse(string src, string result)
		{
			var value = parse(src);
			Assert.AreEqual(result, pp_.Print(value));
		}

		[TestCase("(display 1)")]
		[TestCase("(if #t 1 2)")]
		[TestCase("(define x (lambda (a) (+ 1 a)))")]
		public void TestCompile(string src)
		{
			var compiler = new Compiler();
			var lmd = compiler.Compile(parse(src));
			var code = lmd.Code;
			for( int i = 0; i < code.Length; i++)
			{
				Console.WriteLine("{0:0000}: {1}", i, code[i]);
			}
		}

		public Value Display(params Value[] args)
		{
			foreach (var arg in args)
			{
				Console.Write(arg);
				Console.Write(" ");
			}
			Console.WriteLine("");
			return Value.Nil;
		}

		[TestCase("(display 1)")]
		public void TestRun(string src)
		{
			var compiler = new Compiler();
			var lmd = compiler.Compile(parse(src));
			var eval = new Eval(0);
			var env = new Env(null);
			env.Define(Symbol.Intern("display"), new Value(Display));
			var closure = new Closure(lmd, env);
			eval.Run(closure);
		}
	}
}