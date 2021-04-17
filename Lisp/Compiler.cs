using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
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
		Syn,
		If,
		Ldf,
		Goto,
		Ret,
		Ap,
		Ap1,
		Ccc,
		Max,
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
		Env rootEnv_;

		public Compiler(Vm vm, Env rootEnv)
		{
			vm_ = vm;
			rootEnv_ = rootEnv;
		}

		public Lambda Compile(Value code)
		{
			var ctx = new CompileContext();

			var expandedCode = normalizeSexp(ctx, code);
			//Console.WriteLine($"normalize: {code} ==> {PrettyPrinter.Instance.Print(expandedCode, 1000)}");

			compile(ctx, expandedCode);
			ctx.Emit(Operator.Ret);
			var lmd = new Lambda(C.Nil, ctx.Codes.ToArray(), ctx.Locations.ToArray());

			if (code.IsCons)
			{
				lmd.DefinedLocation = code.AsCons.Location;
			}
			lmd.OriginalSource = code;
			lmd.ExpandedSource = expandedCode;

			vm_.Lambdas.Add(lmd);

			return lmd;
		}

		public Lambda CompileBlock(Value code)
		{
			return Compile(Value.Cons(C.SpBegin, code));
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
			Value.Bind2Rest(code, out _, out param, out body);

			body = Value.Cons(C.SpBegin, body);
			compileForm(newCtx, body);
			newCtx.Emit(Operator.Ret);

			var lmd = new Lambda(param, newCtx.Codes.ToArray(), newCtx.Locations.ToArray());

			lmd.DefinedLocation = code.AsCons.Location;
			lmd.ExpandedSource = body;

			vm_.Lambdas.Add(lmd);
			return lmd;
		}

		void compileForm(CompileContext ctx, Value code)
		{
			var cons = code.AsCons;
			var car = cons.Car;
			var cdr = cons.Cdr;
			ctx.CurrentLocation = code.AsCons.Location;
			//trace("{0} {1}", ctx.CurrentLocation.Line, PrettyPrinter.Instance.Print(code, 30));
			bool normalForm = false;
			if (car.IsSymbol)
			{
				switch (car.AsSymbol.ToString())
				{
					case "%define":
						{
							Value sym, def;
							Value.Bind2(cdr, out sym, out def);
							compile(ctx, def);
							ctx.Emit(Operator.Def, sym);
						}
						break;

					case "%define-syntax":
						{
							Value sym, def;
							Value.Bind2(cdr, out sym, out def);
							compile(ctx, def);
							ctx.Emit(Operator.Syn, sym);
						}
						break;

					case "%set!":
						{
							Value _, sym, def;
							Value.Bind3(code, out _, out sym, out def);
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
							for (var cur = cdr; !cur.IsNil; cur = cur.Cdr)
							{
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
							Value cond, thenBody, elseAndRest;
							Value.Bind2Rest(code.Cdr, out cond, out thenBody, out elseAndRest);

							// else節の処理を行う
							Value elseBody;
							if (!elseAndRest.IsNil)
							{
								elseBody = elseAndRest.Car;
								if( elseAndRest.Cdr != C.Nil)
								{
									throw new LispException("Invalid 'if' form, must has 2 or 3 clauses");
								}
							}
							else
							{
								elseBody = C.Nil;
							}

							compile(ctx, cond);

							var ifPos = ctx.Position;
							ctx.Emit(Operator.If);

							compile(ctx, thenBody);
							var gotoPos = ctx.Position;
							ctx.Emit(Operator.Goto);

							var elsePos = ctx.Position;
							if( !elseBody.IsNil)
							{
								compile(ctx, elseBody);
							}
							else
							{
								ctx.Emit(Operator.Ldc, C.Nil);
							}
							var endPos = ctx.Position;

							ctx.Codes[ifPos] = new Code(Operator.If, new Value(elsePos));
							ctx.Codes[gotoPos] = new Code(Operator.Goto, new Value(endPos));

						}
						break;
					case "apply":
						{
							int len = 0;
							for (var cur = code.Cdr; !cur.IsNil; cur = cur.Cdr, len++)
							{
								compile(ctx, cur.Car);
							}
							ctx.Emit(Operator.Ap1, new Value(len));
						}
						break;
					case "define-library":
						{
							processDefineLibrary(ctx, code);
							ctx.Emit(Operator.Ldc, C.Undef);
						}
						break;
					case "import":
						{
							processImport(ctx, code, rootEnv_);
							ctx.Emit(Operator.Ldc, C.Undef);
						}
						break;
					case "%call-with-current-continuation":
						{
							for (var cur = code.Cdr; !cur.IsNil; cur = cur.Cdr)
							{
								compile(ctx, cur.Car);
							}
							ctx.Emit(Operator.Ccc);
						}
						break;

					default:
						normalForm = true;
						break;
				}
			}
			else
			{
				normalForm = true;
			}

			if (normalForm)
			{
				int len = 0;
				for (var cur = code; !cur.IsNil; cur = cur.Cdr, len++)
				{
					compile(ctx, cur.Car);
				}
				ctx.Emit(Operator.Ap, new Value(len));
			}
		}

		void compile(CompileContext ctx, Value code)
		{
			//printf( "eval: %s\n", v2s(sexp));
			if( code.IsCons)
			{
				compileForm(ctx, code);
			}
			else if( code.IsSymbol || code.IsIdentifer)
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
			if (!s.Car.IsSymbolOrIdentifier) return s;

			string sym = s.Car.GetResolvedSymbol().ToString();
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
					return Value.ConsSrc(s, C.SpDefine,
										 Value.ConsSrc(rest.Car, rest.Car.Car,
													   Value.ConsSrc(rest.Cdr, lambda, C.Nil)));
				}
				else
				{
					throw new LispException("Invalid define form");
				}
			}
			else if (sym == "lambda")
			{
				return Value.ConsSrc(s, C.SpLambda, Value.ConsSrc(rest, rest.Car, normalizeList(ctx, rest.Cdr)));
			}
			else if (sym == "%define-syntax")
			{
				return Value.ConsSrc(s, C.SpDefineSyntax, normalizeList(ctx, rest));

			}
			else if (sym == "if")
			{
				Value _cond, _then, _else;
				Value.Bind2Rest(rest, out _cond, out _then, out _else);
				if (_else == C.Nil) _else = Value.Cons(C.Undef, C.Nil);
				return Value.ConsSrc(s, C.SpIf, Value.Cons(normalizeSexp(ctx, _cond), normalizeSexp(ctx, _then), normalizeList(ctx, _else)));

			}
			else if (sym == "begin")
			{
				if (rest == C.Nil) return C.Undef;
				if (rest.Cdr == C.Nil) return normalizeSexp(ctx, rest.Car);
				return Value.Cons(C.SpBegin, normalizeList(ctx, rest));

			}
			else if (sym == "set!")
			{
				Value sym_, val;
				Value.Bind2(rest, out sym_, out val);
				return Value.ConsSrc(s, C.SpSet, Value.Cons(sym_, normalizeSexp(ctx, val), C.Nil));
			}
			else if (sym == "quote")
			{
				return Value.ConsSrc(s, C.SpQuote, rest);
			}
			else if (sym == "define-library")
			{
				return s;
			}
			else if (sym == "import")
			{
				return s;
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
			if (rootEnv_.TryGet(s.Car.AsSymbol, out v))
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

		//===================================================================
		// define-library
		//===================================================================
		void processDefineLibrary(CompileContext ctx, Value s)
		{
			Value _, moduleNameList, rest;
			Value.Bind2Rest(s, out _, out moduleNameList, out rest);
			string moduleName = Module.GetModuleName(moduleNameList);

			var module = new Module(moduleName);
			var env = new Env(null);

			for (Value codeCons = rest; !codeCons.IsNil; codeCons = codeCons.Cdr)
			{
				var code = codeCons.Car;
				switch( code.Car.AsSymbol.ToString())
				{
					case "import":
						{
							processImport(ctx, code, env);
						}
						break;
					case "export":
						{
							for (Value sym = code.Cdr; !sym.IsNil; sym = sym.Cdr)
							{
								module.Export(sym.Car.AsSymbol);
							}
						}
						break;
					case "include":
					case "include-ci":
						break;
					case "begin":
						{
							var compiler = new Compiler(vm_, env);

							for (Value c = code.Cdr; !c.IsNil; c = c.Cdr)
							{
								var lmd = compiler.Compile(c.Car);
								var closure = new Closure(lmd, env);
								var result = vm_.Eval.Run(closure);
							}
						}
						break;
					default:
						throw new LispException($"Invalid define-library command {code}");
				}
			}

			module.ExportFromEnv(env);

			vm_.Modules[moduleName] = module;
		}

		void processImport(CompileContext ctx, Value s, Env env)
		{
			for (Value codeCons = s.Cdr; !codeCons.IsNil; codeCons = codeCons.Cdr)
			{
				var code = codeCons.Car;
				var importSet = parseImportSet(code);
				importSet.Module.ImportToEnv(env, importSet);
			}
		}

		ImportSet parseImportSet(Value s)
		{
			switch (s.Car.AsSymbol.ToString())
			{
				case "only":
					{
						Value module, symbols;
						Value.Bind1Rest(s.Cdr, out module, out symbols);
						var importSet = parseImportSet(module);
						importSet.Only(Value.ListToArray(symbols).Select(v => v.AsSymbol));
						return importSet;
					}
				case "except":
					throw new NotImplementedException();
				case "rename":
					throw new NotImplementedException();
				case "prefix":
					throw new NotImplementedException();
				default:
					{
						string moduleName = Module.GetModuleName(s);
						Module module;
						if (!vm_.Modules.TryGetValue(moduleName, out module))
						{
							string path = Module.GetModulePath(s);
							var port = new Port(new MemoryStream(File.ReadAllBytes(path)), path);
							vm_.Run(port);
							vm_.Modules.TryGetValue(moduleName, out module);
						}
						return new ImportSet(module);
					}
			}
		}

	}

	public class ImportSet
	{
		public readonly Module Module;
		public Dictionary<Symbol, Symbol> Imports;

		public ImportSet(Module module)
		{
			Module = module;
			Imports = module.Exports.ToDictionary(s => s, s => s);
		}

		public void Only(IEnumerable<Symbol> symbols)
		{
			var symbolsArray = symbols.ToArray();
			Imports = symbols.ToDictionary(s => s, s => s);
		}
	}

}
