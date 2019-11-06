using System;
using System.Collections.Generic;
using System.Text;

namespace Lisp
{
    public class Cons
    {
        Value Car;
        Value Cdr;

        public string SrcFilename { get; private set; }
        public int SrcLine { get; private set; }

        public Cons(Value car, Value cdr)
        {
            Car = car;
            Cdr = cdr;
        }

        public static Value WithLocation(Port src, Value car, Value cdr)
        {
            var cons = new Cons(car, cdr);
            cons.SrcFilename = src.Filename;
            cons.SrcLine = src.Line;
            return new Value(cons);
        }

        public static Value WithLocation(Value src, Value car, Value cdr)
        {
            var cons = new Cons(car, cdr);
            var srcCons = src.As<Cons>();
            cons.SrcFilename = srcCons.SrcFilename;
            cons.SrcLine = srcCons.SrcLine;
            return new Value(cons);
        }
    }

}
