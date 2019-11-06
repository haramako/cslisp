using System;
using Lisp;
using System.IO;
using System.Text;

class Program
{
    static void Main(string[] args)
    {
        var src = "(hoge 1 (2 3))";
        var buf = Encoding.UTF8.GetBytes(src);
        var s = new MemoryStream(buf);
        var port = new Port(s);

        var parser = new Parser();
        parser.Parse(port);
        Console.ReadKey();
    }
}
