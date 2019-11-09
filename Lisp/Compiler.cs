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
		Pop,
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

	public struct SourceLocation
	{
		public string Filename;
		public int Line;
	}

	public class Lambda
	{
		public Code[] Code;
		public SourceLocation[] Locations;
		public Symbol[] Params;

		public Lambda(Value param, Code[] codes, SourceLocation[] locations)
		{
			Code = codes;
			Locations = locations;

			var paramList = new List<Symbol>();
			for( var p = param; !p.IsNil; p = p.Cdr)
			{
				paramList.Add(p.Car.AsSymbol);
			}
			Params = paramList.ToArray();
		}

		public override string ToString()
		{
			return "#<lambda>";
		}
	}

	public class Compiler
	{
		public class CompileContext
		{
			public List<Code> Codes = new List<Code>();
			public List<SourceLocation> Locations = new List<SourceLocation>();
			public SourceLocation CurrentLocation;

			public void Emit(Operator op)
			{
				Codes.Add(new Code(op));
				Locations.Add(CurrentLocation);
			}

			public void Emit(Operator op, Value val)
			{
				Codes.Add(new Code(op, val));
				Locations.Add(CurrentLocation);
			}

			public int Position => Codes.Count;
		}

		Vm vm_;

		public Compiler(Vm vm)
		{
			vm_ = vm;
		}

		public Lambda Compile(Value code)
		{
			var ctx = new CompileContext();

			var oldCode = code;
			code = normalizeSexp(ctx, code);
			Console.WriteLine($"normalize: {oldCode}=>{code}");

			compile(ctx, code);
			ctx.Emit(Operator.Ret);
			return new Lambda(C.Nil, ctx.Codes.ToArray(), ctx.Locations.ToArray());
		}

		public Lambda CompileBlock(Value code)
		{
			return Compile(Value.Cons(Value.Intern("begin"), code));
		}

		//===================================================================
		// Utility
		//===================================================================

		void trace(string format, params object[] param)
		{
			Console.WriteLine(format, param);
		}

		//===================================================================
		// Compile
		//===================================================================

		Lambda compileLambda(CompileContext ctx, Value code)
		{
			var newCtx = new CompileContext();

			Value _, param, body;
			ConsUtil.Bind2Rest(code, out _, out param, out body);

			body = new Value(new Cons(C.Begin, body));
			compileForm(newCtx, body);
			newCtx.Emit(Operator.Ret);
			return new Lambda(param, newCtx.Codes.ToArray(), newCtx.Locations.ToArray());
		}

		void compileForm(CompileContext ctx, Value code)
		{
			var cons = code.AsCons;
			var car = cons.Car;
			var cdr = cons.Cdr;
			ctx.CurrentLocation = code.AsCons.Location;
			if (car.IsSymbol)
			{
				switch (car.AsSymbol.ToString())
				{
					case "%define":
						{
							Value sym, def;
							ConsUtil.Bind2(cdr, out sym, out def);
							compile(ctx, def);
							ctx.Emit(Operator.Def, sym);
						}
						break;

					case "%define-syntax":
						{
							Value sym, def;
							ConsUtil.Bind2(cdr, out sym, out def);
							compile(ctx, def);
							ctx.Emit(Operator.Syntax, sym);
						}
						break;

					case "%set!":
						{
							Value sym, def;
							ConsUtil.Bind2(code, out sym, out def);
							compile(ctx, def);
							ctx.Emit(Operator.Set, sym);
						}
						break;

					case "%quote":
						{
							ctx.Emit(Operator.Ldc, cdr.Car);
						}
						break;

					case "%lambda":
						{
							var lmd = compileLambda(ctx, code);
							ctx.Emit(Operator.Ldf, new Value(lmd));
						}
						break;

					case "%begin":
						{
							for( var cur = cdr; !cur.IsNil; cur = cur.Cdr)
							{
								trace("begin {0}", cur.Car);
								compile(ctx, cur.Car);
								if (!cur.Cdr.IsNil)
								{
									ctx.Emit(Operator.Pop);
								}
							}
						}
						break;
					case "%if":
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
							for (var cur = code; !cur.IsNil; cur = cur.Cdr, len++)
							{
								compile(ctx, cur.Car);
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
				compileForm(ctx, code);
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

		//===================================================================
		// Syntax normalization
		//===================================================================

		Value normalizeSexp(CompileContext ctx, Value s)
		{
			// printf( "s:%s\n", v2s_limit(s,30) );
			if (!s.IsCons) return s;
			if (s.Car.IsCons) return normalizeList(ctx, s);
			if (!s.Car.IsSymbol) return s;

			string sym = s.Car.AsSymbol.ToString();
			Value rest = s.Cdr;
			if (sym == "define")
			{
				if (rest.Car.IsSymbol)
				{
					// (define sym val) の形
					return Value.ConsSrc(s, C.SpDefine, normalizeList(ctx, rest));
				}
				else if (rest.Car.IsCons)
				{
					// (define (sym args ...) ... ) の形
					Value lambda = Value.ConsSrc(s, C.SpLambda,
												 Value.ConsSrc(rest.Car, rest.Car.Cdr, normalizeList(ctx, rest.Cdr)));
					return Value.ConsSrc(s, C.SpLambda,
										 Value.ConsSrc(rest.Car, rest.Car.Car,
													   Value.ConsSrc(rest.Cdr, lambda, C.Nil)));
				}
				else
				{
					throw new LuaException("Invalid define form");
				}
			}
			else if (sym == "lambda")
			{
				return Value.Cons(C.SpLambda, Value.Cons(rest.Car, normalizeList(ctx, rest.Cdr)));
			}
			else if (sym == "define-syntax")
			{
				return Value.ConsSrc(s, C.SpDefineSyntax, normalizeList(ctx, rest));

			}
			else if (sym == "if")
			{
				Value _cond, _then, _else;
				ConsUtil.Bind2Rest(rest, out _cond, out _then, out _else);
				if (_else == C.Nil) _else = Value.Cons(C.Undef, C.Nil);
				return ConsUtil.Cons(C.SpIf, normalizeSexp(ctx, _cond), normalizeSexp(ctx, _then), normalizeList(ctx, _else));

			}
			else if (sym == "begin")
			{
				if (rest == C.Nil) return C.Undef;
				if (rest.Cdr == C.Nil) return normalizeSexp(ctx, rest.Car);
				return ConsUtil.Cons(C.SpBegin, normalizeList(ctx, rest));

			}
			else if (sym == "set!")
			{
				Value sym_, val;
				ConsUtil.Bind2(rest, out sym_, out val);
				return ConsUtil.Cons(C.SpSet, sym_, normalizeSexp(ctx, val));
			}
			else if (sym == "quote")
			{
				return ConsUtil.Cons(C.SpQuote, rest);

			}
			else
			{
				s = normalizeSyntax(ctx, s);
				return normalizeList(ctx, s);
			}
		}

		// implicit begin
		Value normalizeBegin(CompileContext ctx, Value list)
		{
			return normalizeSexp(ctx, Value.ConsSrc(list, C.Begin, list));
		}

		Value normalizeList(CompileContext ctx, Value list)
		{
			if (list.IsCons)
			{
				return Value.ConsSrc(list, normalizeSexp(ctx, list.Car), normalizeList(ctx, list.Cdr));
			}
			else
			{
				return list;
			}
		}

		Value normalizeSyntax(CompileContext ctx, Value s)
		{
			if (!s.Car.IsSymbol) return s;

			Value v;
			if (vm_.RootEnv.TryGet(s.Car.AsSymbol, out v))
			{
				if (!v.IsClosure) return s;

				Closure closure = v.AsClosure;
				if (!closure.IsSyntax)
				{
					return s;
				}

				var expanded = vm_.Apply(closure, s, C.Nil, C.Nil);

				return normalizeSexp(ctx, expanded);
			}
			else
			{
				return s;
			}
		}
	}
}
