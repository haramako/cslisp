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
				case ValueType.Float:
					sb_.Append(v.AsFloat);
					break;
				case ValueType.Symbol:
					sb_.Append(v.AsSymbol.ToString());
					break;
				case ValueType.Char:
					{
						sb_.Append("#\\");
						var c = v.AsChar;
						if (Char.IsControl(c))
						{
							sb_.Append("x");
							sb_.Append(Convert.ToString((int)c, 16));
						}
						else
						{
							sb_.Append(c);
						}
					}
					break;
				case ValueType.String:
					// TODO: escape をしていない
					sb_.Append("\"");
					sb_.Append(v.AsString);
					sb_.Append("\"");
					break;
				case ValueType.Closure:
					// TODO: 未実装
					sb_.Append(v.AsClosure.ToString());
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
				case ValueType.ByteVector:
					{
						var bytes = v.AsByteVector;
						sb_.Append("(bytevector ");
						for (int i = 0; i < bytes.Length; i++ )
						{
							sb_.Append(bytes[i]);
							if (i != bytes.Length - 1)
							{
								sb_.Append(" ");
							}
						}
						sb_.Append(")");
					}
					break;
				case ValueType.Vector:
					{
						var vec = v.AsVector;
						sb_.Append("(vector ");
						for (int i = 0; i < vec.Length; i++)
						{
							generate(vec[i]);
							if (i != vec.Length - 1)
							{
								sb_.Append(" ");
							}
						}
						sb_.Append(")");
					}
					break;
				case ValueType.Object:
					sb_.Append(v.AsObject);
					break;
				default:
					throw new NotImplementedException($"Print for {v.ValueType} not implemented.");
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
