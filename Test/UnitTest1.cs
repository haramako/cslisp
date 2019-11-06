using NUnit.Framework;
using Lisp;
using System.IO;
using System;
using System.Text;

namespace Tests
{
    public class Tests
    {

        [SetUp]
        public void Setup()
        {
        }

        Value parse(string src)
        {
            var s = new MemoryStream(Encoding.UTF8.GetBytes(src));
            var port = new Port(s);
            var parser = new Parser();

            var result = parser.Parse(port);

            Console.WriteLine(result);

            return result;
        }

        [Test]
        public void Test1()
        {
            var value = parse("(1)");

        }
    }
}