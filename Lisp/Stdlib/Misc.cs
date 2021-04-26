using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace Lisp.Stdlib
{
	class Misc
	{
		[LispApi("%load")]
		public static Value load(Context ctx, Value v)
		{
			var src = File.ReadAllBytes(v.AsString);
			var port = new Port(new MemoryStream(src), v.AsString);
			return ctx.Vm.Run(port);
		}

		[LispApi("%include")]
		public static Value include(Context ctx, Value v)
		{
			var src = File.ReadAllBytes(v.AsString);
			var port = new Port(new MemoryStream(src), v.AsString);
			return ctx.Vm.Run(port, ctx.Env);
		}

		[LispApi("%exit")]
		public static Value exit(Context ctx, Value v)
		{
			throw new ExitException(v.AsInt);
		}

		[LispApi("%backtrace")]
		public static Value backtrace(Context ctx)
		{
			ctx.Vm.Eval.ShowBacktrace(null);
			return C.Nil;
		}

		[LispApi("%eval-compile")]
		public static Value eval_compile(Context ctx, Value code, Value env)
		{
			var compiler = new Compiler(ctx.Vm, ctx.Env);
			var lmd = compiler.Compile(code);
			var closure = new Closure(lmd, new Env(env.As<Env>()));
			return new Value(closure);
		}

		[LispApi("%current-filename")]
		public static Value current_filename(Context ctx)
		{
			return new Value(ctx.Vm.CurrentPort.Filename);
		}

		[LispApi]
		public static Value current_environment(Context ctx)
		{
			return new Value(ctx.Env);
		}

		[LispApi]
		public static Value begin_trace(Context ctx)
		{
			ctx.Vm.Eval.Trace = true;
			return C.Nil;
		}

		[LispApi("%dir-name")]
		public static Value dir_name(Context ctx, Value path)
		{
			return new Value(Path.GetDirectoryName(path.AsString));
		}

		[LispApi("assq")]
		public static Value assq(Context ctx, Value key, Value assoc)
		{
			while(!assoc.IsNil)
			{
				if (!assoc.IsCons)
				{
					return C.Nil;
				}
				var cons = assoc.AsCons;
				if(cons.Car.IsCons)
				{
					var kv = cons.Car.AsCons;
					if( kv.Car == key)
					{
						return cons.Car;
					}
				}
				assoc = cons.Cdr;
			}
			return C.Nil;
		}

	}
}
