using System;
using System.Collections.Generic;
using System.Text;

namespace Lisp
{
	public struct SourceLocation
	{
		public readonly string Filename;
		public readonly int Line;

		public SourceLocation(string filename, int line)
		{
			Filename = filename;
			Line = line;
		}

		public string DisplayString => Filename == null ? $"<unknown>:{Line}" : $"{Filename}:{Line}";
	}

	public class Cons
	{
		public Value Car;
		public Value Cdr;
		public readonly SourceLocation Location;

		public Cons(Value car, Value cdr)
		{
			Car = car;
			Cdr = cdr;
		}

		public Cons(SourceLocation location, Value car, Value cdr)
		{
			Location = location;
			Car = car;
			Cdr = cdr;
		}
	}

}
