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
			var lmd = ctx.Vm.Compiler.Compile(code);
			var closure = new Closure(lmd, new Env(env.As<Env>()));
			return new Value(closure);
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

	}
}
