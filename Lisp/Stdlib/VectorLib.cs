using System;
using System.Collections.Generic;
using System.Text;

namespace Lisp.Stdlib
{
	class VectorLib
	{
		[LispApi]
		public static Value vector(Context ctx, Value[] args)
		{
			// TODO: 引数が３個以上ならエラー
			var vec = new Value[args.Length];
			for (int i = 0; i < vec.Length; i++)
			{
				vec[i] = args[i];
			}
			return new Value(vec);
		}

		[LispApi]
		public static Value make_vector(Context ctx, Value[] args)
		{
			// TODO: 引数が３個以上ならエラー
			var len = args[0].AsInt;
			var v = new Value[len];
			if (args.Length >= 2)
			{
				Value fill = args[1];
				for (int i = 0; i < v.Length; i++)
				{
					v[i] = fill;
				}
			}
			return new Value(v);
		}

		[LispApi]
		public static Value vector_length(Context ctx, Value v)
		{
			return new Value(v.Len());
		}

		[LispApi]
		public static Value vector_ref(Context ctx, Value v, Value idx)
		{
			return v.AsVector[idx.AsInt];
		}

		[LispApi("vector-set!")]
		public static Value vector_set(Context ctx, Value v, Value idx, Value val)
		{
			v.AsVector[idx.AsInt] = val;
			return v;
		}

		[LispApi("vector=?")]
		public static Value bytevector_eq(Context ctx, Value v1, Value v2)
		{
			var vec1 = v1.AsVector;
			var vec2 = v2.AsVector;
			if (vec1.Length != vec2.Length)
			{
				return new Value(false);
			}

			for (int i = 0; i < vec1.Length; i++)
			{
				if (Value.Eq(vec1[i], vec2[i]))
				{
					return new Value(false);
				}
			}

			return new Value(true);
		}

		[LispApi("vector-copy!")]
		public static Value vector_copy_inplace(Context ctx, Value src_, Value srcStart_, Value target_, Value targetStart_, Value len_)
		{
			// TODO: srcとtargetが同一だった時も大丈夫なように
			var src = src_.AsVector;
			var srcStart = srcStart_.AsInt;
			var target = target_.AsVector;
			var targetStart = targetStart_.AsInt;
			var len = len_.AsInt;

			for (int i = 0; i < len; i++)
			{
				target[targetStart + i] = src[srcStart + i];
			}

			return target_;
		}

		[LispApi]
		public static Value vector_copy(Context ctx, Value v)
		{
			var src = v.AsVector;
			var vector = new byte[src.Length];
			Array.Copy(src, vector, src.Length);
			return new Value(vector);
		}

		[LispApi("vector->list")]
		public static Value vector_to_list(Context ctx, Value v)
		{
			return Value.ArrayToList(v.AsVector);
		}

		[LispApi("list->vector")]
		public static Value list_to_vector(Context ctx, Value v)
		{
			return new Value(Value.ListToArray(v));
		}
	}
}
