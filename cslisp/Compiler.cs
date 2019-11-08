using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;

namespace Lisp
{
	public enum Operator
	{
		Ld,
		Ldc,
		Def,
		Set,
		Syntax,
		If,
		Ldf,
		Goto,
		Ret,
		Ap,
	}

	public struct Code
	{
		public Operator Op;
		public Value Val;

		public Code(Operator op)
		{
			Op = op;
			Val = C.Nil;
		}

		public Code(Operator op, Value val)
		{
			Op = op;
			Val = val;
		}

		public override string ToString()
		{
			return $"{Op} {Val}";
		}

	}

	public class Lambda
	{
		public Code[] Code;

		public Lambda(Code[] codes)
		{
			Code = codes;
		}

		public override string ToString()
		{
			return "#<lambda>";
		}
	}

	public class Compiler
	{
		string v2s(Value v, int limit = 100)
		{
			return PrettyPrinter.Instance.Print(v, limit);
		}

		void trace(string format, params object[] param)
		{
			Console.WriteLine(format, param);
		}

		void ERROR(string msg)
		{

		}

		public class CompileContext
		{
			public List<Code> Codes = new List<Code>();
			public void Emit(Operator op)
			{
				Codes.Add(new Code(op));
			}
			public void Emit(Operator op, Value val)
			{
				Codes.Add(new Code(op, val));
			}
			public int Position => Codes.Count;
		}

		Lambda compileLambda(CompileContext ctx, Value code)
		{
			var newCtx = new CompileContext();

			Value _, param, body;
			ConsUtil.Bind2Rest(code, out _, out param, out body);

			compile(newCtx, body);
			return new Lambda(newCtx.Codes.ToArray());
		}

		void compileList(CompileContext ctx, Value code)
		{
			var cons = code.AsCons;
			var car = cons.Car;
			var cdr = cons.Cdr;
			if (car.IsSymbol)
			{
				switch (car.AsSymbol.ToString())
				{
					case "define":
						{
							Value sym, def;
							ConsUtil.Bind2(cdr, out sym, out def);
							compile(ctx, def);
							ctx.Emit(Operator.Def, sym);
						}
						break;

					case "define-syntax":
						{
							Value sym, def;
							ConsUtil.Bind2(cdr, out sym, out def);
							compile(ctx, def);
							ctx.Emit(Operator.Syntax, sym);
						}
						break;

					case "set!":
						{
							Value sym, def;
							ConsUtil.Bind2(code, out sym, out def);
							compile(ctx, def);
							ctx.Emit(Operator.Set, sym);
						}
						break;

					case "quote":
						ctx.Emit(Operator.Ldc, cdr);
						break;

					case "lambda":
						{
							var lmd = compileLambda(ctx, code);
							ctx.Emit(Operator.Ldf, new Value(lmd));
						}
						break;

					case "if":
						{
							Value cond, thenBody, elseBody;
							ConsUtil.Bind2Rest(code, out cond, out thenBody, out elseBody);
							compile(ctx, cond);

							var ifPos = ctx.Position;
							ctx.Emit(Operator.If);

							compile(ctx, thenBody);
							var gotoPos = ctx.Position;
							ctx.Emit(Operator.Goto);

							var elsePos = ctx.Position;
							compile(ctx, elseBody);
							var endPos = ctx.Position;

							ctx.Codes[ifPos] = new Code(Operator.If, new Value(elsePos));
							ctx.Codes[gotoPos] = new Code(Operator.Goto, new Value(endPos));

						}
						break;
					default:
						{
							int len = 0;
							for (var cur = code; !cur.IsNil; cur = cur.AsCons.Cdr, len++)
							{
								compile(ctx, cur.AsCons.Car);
							}
							ctx.Emit(Operator.Ap, new Value(len));
						}
						break;
				}
			}
		}

		void compile(CompileContext ctx, Value code)
		{
			//printf( "eval: %s\n", v2s(sexp));
			if( code.IsCons)
			{
				compileList(ctx, code);
			}
			else if( code.IsSymbol)
			{
				ctx.Emit(Operator.Ld, code);
			}
			else
			{
				ctx.Emit(Operator.Ldc, code);
			}
		}

		public Lambda Compile(Value code)
		{
			var ctx = new CompileContext();
			compile(ctx, code);
			ctx.Emit(Operator.Ret);
			return new Lambda(ctx.Codes.ToArray());
		}

		#if false
		Value normalize_sexp(Context ctx, Value s)
		{
			// printf( "s:%s\n", v2s_limit(s,30) );
			if (!IS_PAIR(s)) return s;
			if (IS_PAIR(CAR(s))) return normalize_list(ctx, s);
			if (TYPE_OF(CAR(s)) != TYPE_SYMBOL) return s;

			Symbol sym = V2SYMBOL(CAR(s));
			Value rest = CDR(s);
			if (sym == SYM_DEFINE)
			{
				if (IS_SYMBOL(CAR(rest)))
				{
					// (define sym val) の形
					return cons_src(s, V_DEFINE, normalize_list(ctx, rest));
				}
				else if (IS_PAIR(CAR(rest)))
				{
					// (define (sym args ...) ... ) の形
					Value lambda = cons_src(s, V_LAMBDA,
											cons_src(CAR(rest), CDAR(rest), normalize_list(ctx, CDR(rest))));
					return cons_src(s, V_DEFINE,
									cons_src(CAR(rest), CAAR(rest),
											 cons_src(CDR(rest),
													  lambda, C.Nil)));
				}
				else
				{
					assert(0);
				}
			}
			else if (sym == SYM_LAMBDA)
			{
				return cons3(V_LAMBDA, CAR(rest), normalize_list(ctx, CDR(rest)));
			}
			else if (sym == SYM_DEFINE_SYNTAX2)
			{
				return cons_src(s, V_DEFINE_SYNTAX2, normalize_list(ctx, rest));

			}
			else if (sym == SYM_IF)
			{
				Value _cond, _then, _else;
				bind3cdr(rest, _cond, _then, _else);
				if (_else == C.Nil) _else = cons(V_UNDEF, C.Nil);
				return cons4(V_IF, normalize_sexp(ctx, _cond), normalize_sexp(ctx, _then), normalize_list(ctx, _else));

			}
			else if (sym == SYM_BEGIN)
			{
				if (rest == C.Nil) return V_UNDEF;
				if (CDR(rest) == C.Nil) return normalize_sexp(ctx, CAR(rest));
				return cons(V_BEGIN, normalize_list(ctx, rest));

			}
			else if (sym == SYM_SET_I)
			{
				return cons3(V_SET_I, CAR(rest), normalize_sexp(ctx, CDR(rest)));

			}
			else if (sym == SYM_QUOTE)
			{
				return cons(V_QUOTE, rest);

			}
			else
			{
				s = normalize_syntax(ctx, s);
				return normalize_list(ctx, s);
			}
		}

		// implicit begin
		Value normalize_begin(Context ctx, Value list)
		{
			return normalize_sexp(ctx, cons_src(list, (Value)SYM_BEGIN, list));
		}

		Value normalize_list(Context ctx, Value list)
		{
			if (IS_PAIR(list))
			{
				return cons_src(list, normalize_sexp(ctx, CAR(list)), normalize_list(ctx, CDR(list)));
			}
			else
			{
				return list;
			}
		}

		Value normalize_syntax(Context ctx, Value s)
		{
			if (!IS_SYMBOL(CAR(s))) return s;

			//printf("hoge: %s\n", v2s(bundle_get( ctx->bundle, intern("define-syntax"), C.Nil )));
			Value v = bundle_get(ctx->bundle, V2SYMBOL(CAR(s)), C.Nil);
			if (!IS_LAMBDA(v)) return s;

			Lambda lmd = V2LAMBDA(v);
			if (lmd->type != LAMBDA_TYPE_MACRO)
			{
				return s;
			}

			//printf("normalize_syntax: %s\n", v2s(s));

			s = eval(ctx, cons5(v, cons(V_QUOTE, cons(s, C.Nil)), C.Nil, C.Nil, C.Nil));

			//printf("normalize_syntax2: %s\n", v2s(s));

			return normalize_sexp(ctx, s);
		}
		#endif
	}
}
