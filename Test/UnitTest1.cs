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

        [Test]
        public void Test1()
        {
            var value = parse("(1)");
            pp(value);
        }
    }
}