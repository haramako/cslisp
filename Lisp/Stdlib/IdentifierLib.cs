using System;
using System.Collections.Generic;
using System.Text;

namespace Lisp.Stdlib
{
	public static class IdentifierLib
	{
		[LispApi("identifier=?")]
		public static Value identifier_eq(Context ctx, Value enva_, Value a_, Value envb_, Value b_)
		{
			if( a_.IsSymbolOrIdentifier && b_.IsSymbolOrIdentifier)
			{
				var sym = a_.AsSymbol;
				if( sym.ToString() == "unquote" || sym.ToString() == "unquote-splicing")
				{
					return new Value(b_.AsSymbol == a_.AsSymbol);
				}
			}

			if( a_.IsIdentifer && b_.IsIdentifer)
			{
				var a = a_.AsIdentifier;
				var b = b_.AsIdentifier;
				//return new Value(a.Symbol == b.Symbol);
				return new Value(a.Env == b.Env && a.Symbol == b.Symbol);
			}
			else if( a_.IsIdentifer && b_.IsSymbol)
			{
				var a = a_.AsIdentifier;
				var b = b_.AsSymbol;
				var slot = envb_.As<Env>().GetSlot(b);
				//return new Value(a.Symbol == b);
				return new Value((slot == null || slot == a.Env) && a.Symbol == b);
			}
			else if (b_.IsIdentifer && a_.IsSymbol)
			{
				var a = a_.AsSymbol;
				var b = b_.AsIdentifier;
				var slot = enva_.As<Env>().GetSlot(a);
				//return new Value(b.Symbol == a);
				return new Value((slot == null || slot == b.Env) && b.Symbol == a);
			}
			else
			{
				if( a_.IsIdentifer && b_.IsSymbol)
				{
					//return new Value(a_.AsIdentifier.Symbol == b_.AsSymbol);
				}
				if (b_.IsIdentifer && a_.IsSymbol)
				{
					//return new Value(b_.AsIdentifier.Symbol == a_.AsSymbol);
				}
				return new Value(a_ == b_);
			}
		}

		[LispApi("syntactic-closure-set-rename!")]
		public static Value syntactic_closure_set_rename(Context ctx, Value id, Value renamer)
		{
			id.AsIdentifier.Renamer = renamer;
			return C.Nil;
		}

		[LispApi]
		public static Value make_syntactic_closure(Context ctx, Value env_, Value freevar, Value form)
		{
			var id = new Identifier();
			var symbol = form.AsSymbol;
			id.Symbol = symbol;

			var env = env_.As<Env>();
			if (env != null )
			{
				id.Env = env.GetSlot(symbol);
			}
			else
			{
				id.Env = env;
			}
			return new Value(id);
		}

		[LispApi]
		public static Value current_usage_environment(Context ctx, Value[] args)
		{
			if (args.Length == 0)
			{
				return new Value(ctx.Vm.UsageEnvironment);
			}
			else if (args.Length == 1)
			{
				ctx.Vm.UsageEnvironment = args[0].As<Env>();
				return C.Nil;
			}
			else
			{
				throw new ArgumentException();
			}
		}

		[LispApi]
		public static Value current_transformer_environment(Context ctx, Value[] args)
		{
			if (args.Length == 0)
			{
				return new Value(ctx.Vm.TransformerEnvironment);
			}
			else if (args.Length == 1)
			{
				ctx.Vm.TransformerEnvironment = args[0].As<Env>();
				return C.Nil;
			}
			else
			{
				throw new ArgumentException();
			}
		}

		[LispApi]
		public static Value current_renamer(Context ctx, Value[] args)
		{
			if (args.Length == 0)
			{
				return ctx.Vm.CurrentRenamer;
			}
			else if (args.Length == 1)
			{
				ctx.Vm.CurrentRenamer = args[0];
				return C.Nil;
			}
			else
			{
				throw new ArgumentException();
			}
		}

	}
}
