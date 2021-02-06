using System;
using System.Collections.Generic;
using System.Text;

namespace Lisp.Stdlib
{
	class ByteVectorLib
	{
		[LispApi]
		public static Value bytevector(Context ctx, Value[] args)
		{
			// TODO: 引数が３個以上ならエラー
			var bytes = new byte[args.Length];
			for( int i = 0; i < bytes.Length; i++)
			{
				bytes[i] = (byte)args[i].ConvertToInt();
			}
			return new Value(bytes);
		}

		[LispApi]
		public static Value make_bytevector(Context ctx, Value[] args)
		{
			// TODO: 引数が３個以上ならエラー
			var len = args[0].AsInt;
			var v = new byte[len];
			if (args.Length >= 2)
			{
				byte fill = (byte)args[1].AsInt;
				for (int i = 0; i < v.Length; i++)
				{
					v[i] = fill;
				}
			}
			return new Value(v);
		}

		[LispApi]
		public static Value bytevector_length(Context ctx, Value v)
		{
			return new Value(v.Len());
		}

		[LispApi]
		public static Value bytevector_u8_ref(Context ctx, Value v, Value idx)
		{
			return new Value((int)v.AsByteVector[idx.AsInt]);
		}

		[LispApi("bytevector-u8-set")]
		public static Value bytevector_u8_set(Context ctx, Value v, Value idx, Value val)
		{
			v.AsByteVector[idx.AsInt] = (byte)val.AsInt;
			return v;
		}

		[LispApi("bytevector=?")]
		public static Value bytevector_eq(Context ctx, Value v1, Value v2)
		{
			var b1 = v1.AsByteVector;
			var b2 = v2.AsByteVector;
			if( b1.Length != b2.Length)
			{
				return new Value(false);
			}

			for( int i = 0; i < b1.Length; i++)
			{
				if( b1[i] != b2[i])
				{
					return new Value(false);
				}
			}

			return new Value(true);
		}

		[LispApi("bytevector-copy!")]
		public static Value bytevector_copy_inplace(Context ctx, Value src_, Value srcStart_, Value target_, Value targetStart_, Value len_)
		{
			// TODO: srcとtargetが同一だった時も大丈夫なように
			var src = src_.AsByteVector;
			var srcStart = srcStart_.AsInt;
			var target = target_.AsByteVector;
			var targetStart = targetStart_.AsInt;
			var len = len_.AsInt;

			for (int i = 0; i < len; i++)
			{
				target[targetStart + i] = src[srcStart + i];
			}

			return target_;
		}

		[LispApi]
		public static Value bytevector_copy(Context ctx, Value v)
		{
			var src = v.AsByteVector;
			var vector = new byte[src.Length];
			Array.Copy(src, vector, src.Length);
			return new Value(vector);
		}
	}
}
