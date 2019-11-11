using System;
using System.Collections.Generic;
using System.Text;

namespace Lisp.Stdlib
{
	public static class List
	{
		[LispApi]
		public static Value car(Context ctx, Value v)
		{
			return v.Car;
		}

		[LispApi]
		public static Value cdr(Context ctx, Value v)
		{
			return v.Cdr;
		}

		[LispApi]
		public static Value cons(Context ctx, Value v1, Value v2)
		{
			return Value.Cons(v1, v2);
		}

		[LispApi("set-car!")]
		public static Value set_car(Context ctx, Value pair, Value v)
		{
			pair.AsCons.Car = v;
			return C.Nil;
		}

		[LispApi("set-cdr!")]
		public static Value set_cdr(Context ctx, Value pair, Value v)
		{
			pair.AsCons.Cdr = v;
			return C.Nil;
		}

		[LispApi]
		public static Value list(Context ctx, Value[] args)
		{
			if (args.Length <= 0) return C.Nil;
			if (args.Length <= 1) return Value.Cons(args[0], C.Nil);

			Value cons = Value.Cons(args[0], C.Nil);
			Value cur = cons;
			for( int i = 1; i < args.Length; i++)
			{
				var newCons = Value.Cons(args[i], C.Nil);
				cur.AsCons.Cdr = newCons;
				cur = newCons;
			}
			return cons;
		}

		[LispApi("list*")]
		//[LispApi("cons*")]
		public static Value list_a(Context ctx, Value[] args)
		{
			if (args.Length <= 0) return C.Nil;
			if (args.Length <= 1) return args[0];

			Value cons = Value.Cons(args[0], C.Nil);
			Value cur = cons;
			for (int i = 1; i < args.Length; i++)
			{
				if (i < args.Length - 1)
				{
					var newCons = Value.Cons(args[i], C.Nil);
					cur.AsCons.Cdr = newCons;
					cur = newCons;
				}
				else
				{
					cur.AsCons.Cdr = args[i];
				}
			}
			return cons;
		}

		[LispApi]
		public static Value append(Context ctx, Value[] args)
		{
			return Value.Append(args);
		}

		[LispApi]
		public static Value reverse(Context ctx, Value list)
		{
			return Value.Reverse(list);
		}

	}
}
