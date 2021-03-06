﻿using System;
using System.Collections.Generic;
using System.Text;
using System.Linq;

namespace Lisp
{
	public struct Dump
	{
		public Closure Closure;
		public int Pc;
		public Env Env;
		public int StackSize; // For debug

		public Dump(Closure closure, int pc, Env env, int stackSize)
		{
			Closure = closure;
			Pc = pc;
			Env = env;
			StackSize = stackSize;
		}
	}

	public class EvalStatistics
	{
		public int[] ExecCount = new int[(int)Operator.Max];
		public int MaxStack;
		public int MaxDump;
		public int ApLispCount;
		public int ApNativeCount;
		public Dictionary<Symbol, int> ApplyCount = new Dictionary<Symbol, int>();
	}


	public class Eval
	{
		Context ctx_;

		Closure closure_;

		Stack<Value> stack_ = new Stack<Value>(); // SECDマシンの"S"
		Env env_; // SECDマシンの"E"
		Code[] code_; // SECDマシンの"C"
		int pc_; // SECDマシンの"C"
		Stack<Dump> dump_ = new Stack<Dump>(); // SECDマシンの"D"

		EvalStatistics statistics_ = new EvalStatistics();

		public bool Trace = false;

		public EvalStatistics Statistics => statistics_;

		public Eval(Vm vm)
		{
			ctx_ = new Context(vm);
		}

		static T[] popMulti<T>(Stack<T> stack, int n)
		{
			var r = new T[n];
			for (int i = n - 1; i >= 0; i--)
			{
				r[i] = stack.Pop();
			}
			return r;
		}


		public Value Apply(Closure closure, params Value[] args)
		{
			closure_ = closure;
			var lmd = closure.Lambda;

			env_ = new Env(closure.Env);
			loadParameters(env_, lmd, args);

			code_ = closure.Lambda.Code;
			pc_ = 0;

			return Execute();
		}

		void loadParameters(Env e, Lambda lmd, Value[] args)
		{
			if( args.Length < lmd.Params.Length)
			{
				throw new LispException($"Not enough arguments for {lmd}, expect {lmd.Params.Length} but {args.Length}");
			}

			for (int i = 0; i < lmd.Params.Length; i++)
			{
				e.Define(lmd.Params[i], args[i]);
			}
			if (lmd.RestParam != null)
			{
				var li = C.Nil;
				for (int i = lmd.Params.Length; i < args.Length; i++)
				{
					li = Value.Cons(args[i], li);
				}
				li = Value.ReverseInplace(li);
				e.Define(lmd.RestParam, li);
			}
		}

		public Value Run(Closure closure)
		{
			closure_ = closure;
			code_ = closure.Lambda.Code;
			env_ = closure.Env;
			pc_ = 0;
			return Execute();
		}

		public Value Execute()
		{
			Context ctx = ctx_;

			int pc = pc_;
			Code[] code = code_;

			var closure = closure_;
			var s = stack_;
			Env e = env_;
			var d = dump_;
			SourceLocation location = new SourceLocation();

			EvalStatistics stat = statistics_;

			restart:
			try
			{
				Code c;
				while (true)
				{
					location = closure.Lambda.Locations[pc];
					c = code[pc++];

					stat.ExecCount[(int)c.Op]++;
					stat.MaxStack = Math.Max(stat.MaxStack, s.Count);
					stat.MaxDump = Math.Max(stat.MaxDump, d.Count);


					var l = location;
					if (l.Line == 0)
					{
						l = closure.Lambda.DefinedLocation;
					}
					//Console.WriteLine("{0} {3} at {1}:{2}", c, l.Filename, l.Line, s.Count);
					if (Trace)
					{
						var stackStr = string.Join(", ", s.ToArray().Select(x => x.ToString()));
						Console.WriteLine($"{pc}: {c} {d.Count} [{stackStr}]");
					}

					switch (c.Op)
					{
						case Operator.Ldc:
							s.Push(c.Val);
							break;
						case Operator.Ld:
							{
								if (c.Val.IsSymbol)
								{
									var sym = c.Val.AsSymbol;
									var val = e.Get(sym);
									s.Push(val);
								}
								else
								{
									var id = c.Val.AsIdentifier;
									var val = e.Get(id.Symbol);
									s.Push(val);
								}
							}
							break;
						case Operator.Pop:
							{
								s.Pop();
							}
							break;
						case Operator.Def:
							{
								var val = s.Peek();
								var sym = c.Val.AsSymbol;
								if (val.IsClosure)
								{
									var lmd = val.AsClosure.Lambda;
									lmd.Name = sym;
								}
								e.Define(sym, val);
							}
							break;
						case Operator.Set:
							{
								var val = s.Peek();
								var sym = c.Val.AsSymbol;
								e.Set(sym, val);
							}
							break;
						case Operator.Syn:
							{
								var val = s.Peek();
								var sym = c.Val.AsSymbol;
								val.AsClosure.IsSyntax = true;
								val.AsClosure.Lambda.Name = sym;
								e.Define(sym, val);
							}
							break;
						case Operator.Ldf:
							{
								s.Push(new Value(new Closure(c.Val.As<Lambda>(), e)));
							}
							break;
						case Operator.Goto:
							{
								pc = c.Val.AsInt;
							}
							break;
						case Operator.Ret:
							{
								Dump restored;
								if (d.TryPop(out restored))
								{
									#if DEBUG
									if (restored.StackSize != s.Count - 1)
									{
										Console.WriteLine($"Invalid stack size {restored.StackSize} {s.Count - 1}");
									}
									#endif
									closure = restored.Closure;
									pc = restored.Pc;
									e = restored.Env;
									code = closure.Lambda.Code;
								}
								else
								{
									saveRegisters(pc, code, closure, s, e, d);
									return s.Pop();
								}
							}
							break;
						case Operator.If:
							{
								var val = s.Pop();
								if (val.IsNil || val == Value.F)
								{
									pc = c.Val.AsInt;
								}
							}
							break;
						case Operator.Ccc:
							{
								var cc = makeContinuation(ref pc, ref code, ref closure, ref s, ref e, ref d);
								var f = s.Pop();
								applyClosure(f.AsClosure, new Value[] { cc }, ref pc, ref code, ref closure, ref s, ref e, ref d);
							}
							break;
						case Operator.Ap:
						case Operator.Ap1:
							{
								int len;
								Value[] args;
								if (c.Op == Operator.Ap)
								{
									len = c.Val.AsInt;
									args = popMulti(s, len - 1);
								}
								else
								{
									var tmpLen = c.Val.AsInt - 1 - 1; // applicant と tail
									if (tmpLen >= 0)
									{
										var tail = Value.ListToArray(s.Pop());
										var tmpArgs = popMulti(s, tmpLen);

										args = new Value[tmpLen + tail.Length];
										len = args.Length;
										for (int i = 0; i < tmpLen; i++)
										{
											args[i] = tmpArgs[i];
										}
										for (int i = 0; i < tail.Length; i++)
										{
											args[tmpLen + i] = tail[i];
										}
									}
									else
									{
										args = new Value[0];
									}
								}
								var applicant = s.Pop();
								var vt = applicant.ValueType;
								if (vt == ValueType.Closure)
								{
									stat.ApLispCount++;
									d.Push(new Dump(closure, pc, e, s.Count));
									var cl = applicant.AsClosure;
									var lmd = cl.Lambda;
									if (lmd.Name != null)
									{
										stat.ApplyCount[lmd.Name] = stat.ApplyCount.GetValueOrDefault(lmd.Name, 0) + 1;
									}
									e = new Env(cl.Env);
									code = cl.Lambda.Code;
									closure = cl;

									loadParameters(e, lmd, args);

									pc = 0;
								}
								else if (vt == ValueType.LispApi)
								{
									stat.ApNativeCount++;
									var func = applicant.AsLispApi;
									if (func.Name != null)
									{
										stat.ApplyCount[func.Name] = stat.ApplyCount.GetValueOrDefault(func.Name, 0) + 1;
									}
									ctx.Env = e;

									if(func.Arity >= 0)
									{
										if( args.Length < func.Arity)
										{
											throw new LispException($"Invalid argument length, expect {func.Arity} but {args.Length} for {func.Func}");
										}
									}

									Value result;
									switch (func.Arity)
									{
										case 0:
											result = ((LispApi.Func0)func.Func)(ctx);
											break;
										case 1:
											result = ((LispApi.Func1)func.Func)(ctx, args[0]);
											break;
										case 2:
											result = ((LispApi.Func2)func.Func)(ctx, args[0], args[1]);
											break;
										case 3:
											result = ((LispApi.Func3)func.Func)(ctx, args[0], args[1], args[2]);
											break;
										case 4:
											result = ((LispApi.Func4)func.Func)(ctx, args[0], args[1], args[2], args[3]);
											break;
										case 5:
											result = ((LispApi.Func5)func.Func)(ctx, args[0], args[1], args[2], args[3], args[4]);
											break;
										default:
											result = ((LispApi.FuncVararg)func.Func)(ctx, args);
											break;
									}
									s.Push(result);
								}
								else if (vt == ValueType.Continuation)
								{
									applyContinuation(applicant.AsContinuation, args, ref pc, ref code, ref closure, ref s, ref e, ref d);
								}
								else
								{
									throw new Exception($"Can't apply {applicant}");
								}
							}
							break;
						default:
							throw new Exception("BUG");
					}
				}
			}
			catch (ExitException ex)
			{
				throw;
			}
			#if !DONT_CATCH_ERROR
			catch (LispException ex)
			{
				// Convert exception to scheme error.
				ex.SetLocation(location);


				Closure errorFunc = null;
				Value found;
				if (e.TryGet(Symbol.Intern("error"), out found))
				{
					if( found.IsClosure)
					{
						errorFunc = found.AsClosure;
					}
				}

				if (errorFunc != null)
				{
					applyClosure(errorFunc, new Value[] { new Value(ex.Message) }, ref pc, ref code, ref closure, ref s, ref e, ref d);
					goto restart;
				}
				else
				{
					saveRegisters(pc, code, closure, s, e, d);
					ShowBacktrace(ex);
					throw;
				}
			}
			#endif
		}

		void saveRegisters(int pc, Code[] code, Closure closure, Stack<Value> stack, Env env, Stack<Dump> dump)
		{
			pc_ = pc;
			code_ = code;
			closure_ = closure;
			stack_ = stack;
			env_ = env;
			dump_ = dump;
		}

		void restoreRegisters(out int pc, out Code[] code, out Closure closure, out Stack<Value> stack, out Env env, out Stack<Dump> dump)
		{
			pc = pc_;
			code = code_;
			closure = closure_;
			stack = stack_;
			env = env_;
			dump = dump_;
		}

		Value makeContinuation(ref int pc, ref Code[] code, ref Closure closure, ref Stack<Value> stack, ref Env env, ref Stack<Dump> dump)
		{
			var cc = new Continuation
			{
				Pc = pc,
				Code = code,
				Closure = closure,
				Stack = new Stack<Value>(stack.ToArray().Reverse().ToArray()),
				Env = env,
				Dump = new Stack<Dump>(dump.ToArray().Reverse().ToArray()),
			};
			return new Value(cc);
		}

		void applyContinuation(Continuation cont, Value[] args, ref int pc, ref Code[] code, ref Closure closure, ref Stack<Value> stack, ref Env env, ref Stack<Dump> dump)
		{
			pc = cont.Pc;
			code = cont.Code;
			closure = cont.Closure;
			stack = cont.Stack;
			env = cont.Env;
			dump = cont.Dump;
			stack.Pop(); // f をなくす
			stack.Push(args[0]);
		}

		void applyClosure(Closure newClosure, Value[] args, ref int pc, ref Code[] code, ref Closure closure, ref Stack<Value> stack, ref Env env, ref Stack<Dump> dump)
		{
			dump.Push(new Dump(closure, pc, env, stack.Count));

			statistics_.ApLispCount++;
			pc = 0;
			var cl = newClosure;
			var lmd = cl.Lambda;
			env = new Env(cl.Env);
			code = cl.Lambda.Code;
			closure = cl;

			loadParameters(env, lmd, args);
		}

		public void ShowBacktrace(Exception ex)
		{
			if (ex != null)
			{
				var l = closure_.Lambda.Locations[pc_ - 1];
				if (l.Line == 0)
				{
					l = closure_.Lambda.DefinedLocation;
				}
				Console.WriteLine("{0}: error {1}", l.DisplayString, ex.Message);
			}

			while (dump_.Count > 0)
			{
				var curDump = dump_.Pop();
				var stackLmd = curDump.Closure.Lambda;
				var l = stackLmd.Locations[curDump.Pc - 1];
				if (l.Line == 0)
				{
					l = curDump.Closure.Lambda.DefinedLocation;
				}
				Console.WriteLine("{0}: in {1}", l.DisplayString, stackLmd);
			}
		}
	}
}
