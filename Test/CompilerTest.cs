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

		Lambda compile(string src)
		{
			var s = new MemoryStream(Encoding.UTF8.GetBytes(src));
			var port = new Port(s);
			var parser = new Parser();

			var code = parser.ParseList(port);
			code = new Value(new Cons(C.Begin, code));

			var vm = new Vm();
			var compiler = new Compiler(vm);
			var lmd = compiler.Compile(code);

			Console.WriteLine(code);
			for (int i = 0; i < lmd.Code.Length; i++)
			{
				Console.WriteLine("{0:0000}: {1}", i, lmd.Code[i]);
			}

			return lmd;
		}

		// テスト
		[TestCase("()", "()")]
		[TestCase("(1)", "(1)")]
		[TestCase("(1 2)", "(2 1)")]
		[TestCase("(1 2 3)", "(3 2 1)")]
		public void TestReverseInplace(string src, string expected)
		{
			var list = parse(src);
			list = Value.ReverseInplace(list);
			Assert.AreEqual(expected, list.ToString());
		}


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
			#if false
			var vm = new Vm();
			var closure = vm.Compile(src);
			var code = closure.Lambda.Code;
			for( int i = 0; i < code.Length; i++)
			{
				Console.WriteLine("{0:0000}: {1}", i, code[i]);
			}
			#endif
		}

		[TestCase("(puts 1)")]
		[TestCase("(define +1 \n(lambda (n) (+ 1 n ))) \n(+1 2)")]
		[TestCase("(%define-syntax a (lambda (c a b) '(display 1))) \n(a 1)")]
		public void TestRun(string src)
		{
			var vm = new Vm();
			var result = vm.Run(src);
			Console.WriteLine(result);
		}

		[TestCase("#f", "#f", true)]
		[TestCase("#t", "#t", true)]
		[TestCase("#t", "#f", false)]
		public void TestValueEqual(string src1, string src2, bool equals)
		{
			var v1 = parse(src1);
			var v2 = parse(src2);
			Assert.AreEqual(equals, src1 == src2);
		}
	}
}