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

			var compiler = new Compiler();
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
			list = ConsUtil.ReverseInplace(list);
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
			for( int i = 0; i < args.Length; i++)
			{
				Console.Write(args[i]);
				if( i < args.Length - 1 )
				{
					Console.Write(" ");
				}
			}
			return Value.Nil;
		}

		public Value Puts(params Value[] args)
		{
			Display(args);
			Console.WriteLine();
			return Value.Nil;
		}

		public Value Begin(params Value[] args)
		{
			return args[args.Length - 1];
		}

		public Value Add(params Value[] args)
		{
			var r = new Value(0);
			for( int i = 0; i < args.Length; i++)
			{
				r.AsInt = r.AsInt + args[i].AsInt;
			}
			return r;
		}

		[TestCase("(puts 1)")]
		[TestCase("(define +1 (lambda (n) (+ 1 n ))) (+1 2)")]
		public void TestRun(string src)
		{
			var lmd = compile(src);

			var eval = new Eval(0);
			var env = new Env(null);
			env.Define(Symbol.Intern("puts"), new Value(Puts));
			env.Define(Symbol.Intern("display"), new Value(Display));
			env.Define(Symbol.Intern("begin"), new Value(Begin));
			env.Define(Symbol.Intern("+"), new Value(Add));
			var closure = new Closure(lmd, env);

			var result = eval.Run(closure);
			Console.WriteLine(result);
		}
	}
}