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
        public void Test1(string src, string result)
        {
            var value = parse(src);
            Assert.AreEqual(result, pp_.Print(value));
        }
    }
}