using System;
using System.Collections.Generic;
using System.Text;
using System.Diagnostics;

namespace Lisp
{
	public class PrettyPrinter
	{
		class LengthLimitException : LispException { }

		public static PrettyPrinter Instance = new PrettyPrinter();

		StringBuilder sb_ = new StringBuilder();
		int limit_ = 2000;

		public PrettyPrinter()
		{
		}

		void checkLimit()
		{
			if( sb_.Length >= limit_)
			{
				throw new LengthLimitException();
			}
		}

		void generate(Value v)
		{
			switch (v.ValueType)
			{
				case ValueType.Nil:
					sb_.Append("()");
					break;
				case ValueType.Bool:
					sb_.Append(v.AsBool ? "#t" : "#f");
					break;
				case ValueType.Integer:
					sb_.Append(v.AsInt);
					break;
				case ValueType.Symbol:
					sb_.Append(v.AsSymbol.ToString());
					break;
				case ValueType.String:
					// TODO: escape をしていない
					sb_.Append("\"");
					sb_.Append(v.AsString);
					sb_.Append("\"");
					break;
				case ValueType.Closure:
					// TODO: 未実装
					sb_.Append("#<");
					sb_.Append(v.AsClosure.Lambda.Name);
					sb_.Append(">");
					break;
				case ValueType.LispApi:
					{
						// TODO: 未実装
						sb_.Append("#<lambda>");
					}
					break;
				case ValueType.Cons:
					sb_.Append("(");
					checkLimit();
					bool finished = false;
					while (!finished)
					{
						var cons = v.AsCons;
						switch (cons.Cdr.ValueType)
						{
							case ValueType.Nil:
								generate(cons.Car);
								finished = true;
								break;
							case ValueType.Cons:
								generate(cons.Car);
								sb_.Append(" ");
								break;
							default:
								generate(cons.Car);
								checkLimit();
								sb_.Append(" . ");
								checkLimit();
								generate(cons.Cdr);
								finished = true;
								break;
						}
						v = cons.Cdr;
					}
					sb_.Append(")");
					break;
				case ValueType.Object:
					sb_.Append(v.AsObject);
					break;
				default:
					break;
			}
		}

		public StringBuilder PrintAsStringBuilder(Value v, int limit = -1)
		{
			if (limit >= 0)
			{
				limit_ = limit;
			}
			sb_.Clear();

			try
			{
				generate(v);
			}
			catch (LengthLimitException)
			{
				sb_.Append("...");
			}
			return sb_;
		}

		public string Print(Value v, int limit = -1)
		{
			return PrintAsStringBuilder(v, limit).ToString();
		}
	}
}
