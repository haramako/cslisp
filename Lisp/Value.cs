﻿using System;
using System.Diagnostics;
using System.Runtime.CompilerServices;

namespace Lisp
{
	public enum ValueType
	{
		Nil,
		Integer,
		Float,
		Bool,
		Char,
		Symbol,
		Reference,

		Cons,
		String,
		ByteVector,
		Vector,
		Table,
		Object,

		Closure,
		LispApi,
		Continuation,
		Identifier,
	}

	public partial struct Value : IEquatable<Value>
	{
		const ulong NanBoxingMask = 0xffff000000000000;
		const ulong ValueMask = 0x0000ffffffffffff;
		const ulong SubValueTypeMask = 0xff;
		const ulong NanBits = 0x7ff8000000000000;
		const ulong NonFloatBits = 0xfff0000000000000;
		const ulong SignMask = 0x0000800000000000;
		const ulong MinusBits = 0xffff000000000000;

		const ulong NilMark = (((ulong)ValueType.Nil) << 48) | NonFloatBits;
		const ulong IntegerMark = (((ulong)ValueType.Integer) << 48) | NonFloatBits;
		const ulong BoolMark = (((ulong)ValueType.Bool) << 48) | NonFloatBits;
		const ulong ReferenceMark = (((ulong)ValueType.Reference) << 48) | NonFloatBits;
		const ulong SymbolMark = ReferenceMark | (ulong)ValueType.Symbol;
		const ulong CharMark = (((ulong)ValueType.Char) << 48) | NonFloatBits;
		const ulong ConsMark = ReferenceMark | (ulong)ValueType.Cons;
		const ulong StringMark = ReferenceMark | (ulong)ValueType.String;
		const ulong ByteVectorMark = ReferenceMark | (ulong)ValueType.ByteVector;
		const ulong VectorMark = ReferenceMark | (ulong)ValueType.Vector;
		const ulong TableMark = ReferenceMark | (ulong)ValueType.Table;
		const ulong ClosureMark = ReferenceMark | (ulong)ValueType.Closure;
		const ulong LispApiMark = ReferenceMark | (ulong)ValueType.LispApi;
		const ulong ContinuationMark = ReferenceMark | (ulong)ValueType.Continuation;
		const ulong IdentifierMark = ReferenceMark | (ulong)ValueType.Identifier;
		const ulong ObjectMark = ReferenceMark | (ulong)ValueType.Object;

		// type用の16bit(bit63..48)の情報
		const int Type16Mask = 0x0007;
		const int Type16FloatBits = 0xfff8;
		const int Type16NotFloat = 0x7ff8;
		const int Type16Nan = 0x0008;


		ulong val_;
		object obj_;

		public static readonly Value Nil = new Value()
		{
			val_ = NilMark, obj_ = null
		};

		[MethodImpl(MethodImplOptions.AggressiveInlining)]
		public Value(Value v)
		{
			val_ = v.val_;
			obj_ = v.obj_;
		}

		[MethodImpl(MethodImplOptions.AggressiveInlining)]
		public Value(int v)
		{
			val_ = 0;
			obj_ = null;
			AsInt = v;
		}

		[MethodImpl(MethodImplOptions.AggressiveInlining)]
		public Value(double v)
		{
			val_ = 0;
			obj_ = null;
			AsFloat = v;
		}

		[MethodImpl(MethodImplOptions.AggressiveInlining)]
		public Value(Symbol v)
		{
			val_ = 0;
			obj_ = null;
			AsSymbol = v;
		}

		[MethodImpl(MethodImplOptions.AggressiveInlining)]
		public Value(char v)
		{
			val_ = 0;
			obj_ = null;
			AsChar = v;
		}

		[MethodImpl(MethodImplOptions.AggressiveInlining)]
		public Value(Cons v)
		{
			if (v == null)
			{
				val_ = NilMark;
				obj_ = null;
			}
			else
			{
				val_ = ConsMark;
				obj_ = v;
			}
		}

		[MethodImpl(MethodImplOptions.AggressiveInlining)]
		public Value(string v)
		{
			if (v == null)
			{
				val_ = NilMark;
				obj_ = null;
			}
			else
			{
				val_ = StringMark;
				obj_ = v;
			}
		}

		[MethodImpl(MethodImplOptions.AggressiveInlining)]
		public Value(byte[] v)
		{
			if (v == null)
			{
				val_ = NilMark;
				obj_ = null;
			}
			else
			{
				val_ = ByteVectorMark;
				obj_ = v;
			}
		}

		[MethodImpl(MethodImplOptions.AggressiveInlining)]
		public Value(Value[] v)
		{
			if (v == null)
			{
				val_ = NilMark;
				obj_ = null;
			}
			else
			{
				val_ = VectorMark;
				obj_ = v;
			}
		}

		[MethodImpl(MethodImplOptions.AggressiveInlining)]
		public Value(bool v)
		{
			val_ = (v ? (ulong)1 : (ulong)0) | BoolMark;
			obj_ = null;
		}

		[MethodImpl(MethodImplOptions.AggressiveInlining)]
		public Value(Table v)
		{
			if (v == null)
			{
				val_ = NilMark;
				obj_ = null;
			}
			else
			{
				val_ = TableMark;
				obj_ = v;
			}
		}

		[MethodImpl(MethodImplOptions.AggressiveInlining)]
		public Value(Closure v)
		{
			if (v == null)
			{
				val_ = NilMark;
				obj_ = null;
			}
			else
			{
				val_ = ClosureMark;
				obj_ = v;
			}
		}

		[MethodImpl(MethodImplOptions.AggressiveInlining)]
		public Value(LispApi v)
		{
			if (v == null)
			{
				val_ = NilMark;
				obj_ = null;
			}
			else
			{
				val_ = LispApiMark;
				obj_ = v;
			}
		}

		[MethodImpl(MethodImplOptions.AggressiveInlining)]
		public Value(Continuation v)
		{
			if (v == null)
			{
				val_ = NilMark;
				obj_ = null;
			}
			else
			{
				val_ = ContinuationMark;
				obj_ = v;
			}
		}

		[MethodImpl(MethodImplOptions.AggressiveInlining)]
		public Value(Identifier v)
		{
			if (v == null)
			{
				val_ = NilMark;
				obj_ = null;
			}
			else
			{
				val_ = IdentifierMark;
				obj_ = v;
			}
		}

		[MethodImpl(MethodImplOptions.AggressiveInlining)]
		public Value(object v)
		{
			val_ = 0;
			obj_ = null;
			AsObject = v;
		}

		[Conditional("DEBUG")]
		public void check(bool cond)
		{
			if (!cond)
			{
				throw new Exception("assert failed!");
			}
		}

		[Conditional("DEBUG")]
		public void checkType(ValueType t)
		{
			if( ValueType != t)
			{
				throw new LispException($"Type error expect {t}, but {ValueType}");
			}
		}

		public ValueType ValueType
		{
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			get
			{
				if (val_ < NonFloatBits)
				{
					return ValueType.Float;
				}
				else
				{
					var type16 = (int)((val_ & NanBoxingMask) >> 48);
					var type = (ValueType)(type16 & Type16Mask);
					if (type == ValueType.Reference)
					{
						return (ValueType)(val_ & SubValueTypeMask);
					}
					else
					{
						return type;
					}
				}
			}
		}

		[MethodImpl(MethodImplOptions.AggressiveInlining)]
		public void Clear()
		{
			val_ = NilMark;
			obj_ = null;
		}

		public bool IsNil
		{
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			get
			{
				return (ValueType == ValueType.Nil);
			}
		}

		public bool IsBool
		{
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			get
			{
				return ValueType == ValueType.Bool;
			}
		}

		public bool IsInteger
		{
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			get
			{
				return ValueType == ValueType.Integer;
			}
		}

		public bool IsFloat
		{
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			get
			{
				return ValueType == ValueType.Float;
			}
		}

		public bool IsNumber
		{
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			get
			{
				return ValueType == ValueType.Integer || ValueType == ValueType.Float;
			}
		}

		public bool IsSymbol
		{
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			get
			{
				return ValueType == ValueType.Symbol;
			}
		}

		public bool IsChar
		{
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			get
			{
				return ValueType == ValueType.Char;
			}
		}

		public bool IsString
		{
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			get
			{
				return ValueType == ValueType.String;
			}
		}

		public bool IsByteVector
		{
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			get
			{
				return ValueType == ValueType.ByteVector;
			}
		}

		public bool IsVector
		{
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			get
			{
				return ValueType == ValueType.Vector;
			}
		}

		public bool IsCons
		{
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			get
			{
				return ValueType == ValueType.Cons;
			}
		}

		public bool IsTable
		{
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			get
			{
				return ValueType == ValueType.Table;
			}
		}

		public bool IsClosure
		{
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			get
			{
				return ValueType == ValueType.Closure;
			}
		}

		public bool IsLispApi
		{
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			get
			{
				return ValueType == ValueType.LispApi;
			}
		}

		public bool IsContinuation
		{
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			get
			{
				return ValueType == ValueType.Continuation;
			}
		}

		public bool IsIdentifer
		{
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			get
			{
				return ValueType == ValueType.Identifier;
			}
		}

		[MethodImpl(MethodImplOptions.AggressiveInlining)]
		public bool Is<T>() where T : class
		{
			return (ValueType == ValueType.Object) && (obj_ is T);
		}

		// TODO: 48bit以上の扱い
		public int AsInt
		{
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			get
			{
				checkType(ValueType.Integer);
				ulong result = val_ & ValueMask;
				if ((result & SignMask) != 0)
				{
					return (int)(result | MinusBits);
				}
				else
				{
					return (int)result;
				}
			}
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			set
			{
				val_ = ((ulong)value & ValueMask) | IntegerMark;
			}
		}

		public double AsFloat
		{
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			get
			{
				checkType(ValueType.Float);
				return BitConverter.Int64BitsToDouble((long)val_);
			}
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			set
			{
				if (double.IsNaN(value))
				{
					val_ = NanBits;
				}
				else
				{
					val_ = (ulong)BitConverter.DoubleToInt64Bits(value);
				}
			}
		}

		public bool AsBool
		{
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			get
			{
				checkType(ValueType.Bool);
				ulong result = val_ & ValueMask;
				return result != 0;
			}
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			set
			{
				val_ = (value ? (ulong)1 : (ulong)0) | BoolMark;
			}
		}

		public char AsChar
		{
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			get
			{
				checkType(ValueType.Char);
				ulong result = val_ & ValueMask;
				if ((result & SignMask) != 0)
				{
					return (char)(result | MinusBits);
				}
				else
				{
					return (char)result;
				}
			}
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			set
			{
				val_ = ((ulong)value & ValueMask) | CharMark;
			}
		}

		public Cons AsCons
		{
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			get
			{
				checkType(ValueType.Cons);
				return (Cons)obj_;
			}
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			set
			{
				val_ = ConsMark;
				obj_ = value;
			}
		}

		public Symbol AsSymbol
		{
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			get
			{
				checkType(ValueType.Symbol);
				return (Symbol)obj_;
			}
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			set
			{
				val_ = SymbolMark;
				obj_ = value;
			}
		}

		public string AsString
		{
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			get
			{
				checkType(ValueType.String);
				return (string)obj_;
			}
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			set
			{
				val_ = StringMark;
				obj_ = value;
			}
		}

		public byte[] AsByteVector
		{
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			get
			{
				checkType(ValueType.ByteVector);
				return (byte[])obj_;
			}
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			set
			{
				val_ = ByteVectorMark;
				obj_ = value;
			}
		}

		public Value[] AsVector
		{
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			get
			{
				checkType(ValueType.Vector);
				return (Value[])obj_;
			}
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			set
			{
				val_ = VectorMark;
				obj_ = value;
			}
		}

		public Table AsTable
		{
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			get
			{
				checkType(ValueType.Table);
				return (Table)obj_;
			}
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			set
			{
				val_ = TableMark;
				obj_ = value;
			}
		}

		public object AsObject
		{
			get
			{
				checkType(ValueType.Object);
				return obj_;
			}
			set
			{
				if (value == null)
				{
					val_ = NilMark;
					obj_ = null;
				}
				else
				{
					val_ = ObjectMark;
					obj_ = value;
				}
			}
		}

		public Closure AsClosure
		{
			get
			{
				checkType(ValueType.Closure);
				return (Closure)obj_;
			}
			set
			{
				if (value == null)
				{
					val_ = NilMark;
					obj_ = null;
				}
				else
				{
					val_ = ClosureMark;
					obj_ = value;
				}
			}
		}

		public LispApi AsLispApi
		{
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			get
			{
				checkType(ValueType.LispApi);
				return (LispApi)obj_;
			}
		}

		public Continuation AsContinuation
		{
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			get
			{
				checkType(ValueType.Continuation);
				return (Continuation)obj_;
			}
		}

		public Identifier AsIdentifier
		{
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			get
			{
				checkType(ValueType.Identifier);
				return (Identifier)obj_;
			}
		}

		public T As<T>() where T: class
		{
			if (IsNil)
			{
				return null;
			}
			else
			{
				checkType(ValueType.Object);
				return (T)obj_;
			}
		}

		//====================================================
		// Lua operators
		//====================================================
		#if false
		public static LuaValue BinOp(OpCode opcode, LuaValue a, LuaValue b)
		{
			bool intOperator; // 0=数値演算, 1=Int演算, 2=Bool演算
			switch (opcode)
			{
				case OpCode.ADD:
				case OpCode.SUB:
				case OpCode.MUL:
				case OpCode.MOD:
				case OpCode.POW:
				case OpCode.DIV:
					intOperator = false;
					break;
				case OpCode.IDIV:
				case OpCode.BAND:
				case OpCode.BOR:
				case OpCode.BXOR:
				case OpCode.SHL:
				case OpCode.SHR:
					intOperator = true;
					break;
				default:
					throw new Exception("invalid opcode " + opcode);
			}

			if (!intOperator)
			{
				var fa = a.ConvertToFloat();
				var fb = b.ConvertToFloat();
				double r = 0;
				switch (opcode)
				{
					case OpCode.ADD:
						r = fa + fb;
						break;
					case OpCode.SUB:
						r = fa - fb;
						break;
					case OpCode.MUL:
						r = fa * fb;
						break;
					case OpCode.MOD:
						r = fa % fb;
						break;
					case OpCode.POW:
						r = Math.Pow(fa, fb);
						break;
					case OpCode.DIV:
						r = fa / fb;
						break;
				}
				return new LuaValue(r);
			}
			else
			{
				var ia = a.ConvertToInt();
				var ib = b.ConvertToInt();
				int r = 0;
				switch (opcode)
				{
					case OpCode.IDIV:
						r = ia % ib;
						break;
					case OpCode.BAND:
						r = ia & ib;
						break;
					case OpCode.BOR:
						r = ia | ib;
						break;
					case OpCode.BXOR:
						r = ia ^ ib;
						break;
					case OpCode.SHL:
						r = ia << ib;
						break;
					case OpCode.SHR:
						r = ia >> ib;
						break;
				}
				return new LuaValue(r);
			}
		}

		public static LuaValue UnaryOp(OpCode opcode, LuaValue a)
		{
			switch (opcode)
			{
				case OpCode.UNM:
					if (a.ValueType == ValueType.Integer)
					{
						return new LuaValue(-a.ConvertToInt());
					}
					else
					{
						return new LuaValue(-a.ConvertToFloat());
					}
				case OpCode.BNOT:
					return new LuaValue(~a.ConvertToInt());
				case OpCode.NOT:
					return new LuaValue(!a.ConvertToBool());
				default:
					throw new Exception("invalid opcode " + opcode);
			}
		}

		public static bool CompOp(OpCode opcode, LuaValue a, LuaValue b)
		{
			switch (opcode)
			{
				case OpCode.EQ:
					return a == b;
				case OpCode.LT:
					if (a.ValueType == ValueType.Integer && b.ValueType == ValueType.Integer)
					{
						return a.ConvertToInt() < b.ConvertToInt();
					}
					else
					{
						return a.ConvertToFloat() < b.ConvertToFloat();
					}
				case OpCode.LE:
					if (a.ValueType == ValueType.Integer && b.ValueType == ValueType.Integer)
					{
						return a.ConvertToInt() <= b.ConvertToInt();
					}
					else
					{
						return a.ConvertToFloat() <= b.ConvertToFloat();
					}
				default:
					throw new Exception("invalid opcode " + opcode);
			}
		}
		public static LuaValue Add(LuaValue a, LuaValue b)
		{
			return LuaValue.Nil;
		}
		#endif

		public int Len()
		{
			switch (ValueType)
			{
				case ValueType.String:
					return AsString.Length;
				case ValueType.ByteVector:
					return AsByteVector.Length;
				case ValueType.Vector:
					return AsVector.Length;
				case ValueType.Table:
					return AsTable.ArraySize;
				default:
					throw new LispException("attempt to get length of " + ToString());
			}
		}

		[MethodImpl(MethodImplOptions.AggressiveInlining)]
		public int ConvertToInt()
		{
			switch (ValueType)
			{
				case ValueType.Integer:
					return AsInt;
				case ValueType.Float:
					return (int)Math.Round(AsFloat);
				default:
					throw new LispException(ToString() + " cannot convert to int");
			}
		}

		[MethodImpl(MethodImplOptions.AggressiveInlining)]
		public double ConvertToFloat()
		{
			switch (ValueType)
			{
				case ValueType.Integer:
					return AsInt;
				case ValueType.Float:
					return AsFloat;
				default:
					throw new LispException(ToString() + " cannot convert to int");
			}
		}

		/// <summary>
		/// boolに変換する
		/// </summary>
		/// <returns><c>true</c>, if to bool was converted, <c>false</c> otherwise.</returns>
		[MethodImpl(MethodImplOptions.AggressiveInlining)]
		public bool ConvertToBool()
		{
			switch (ValueType)
			{
				case ValueType.Bool:
					return AsBool;
				default:
					return !IsNil;
			}
		}

		//====================================================
		// Operators
		//====================================================

		public override string ToString()
		{
			return PrettyPrinter.Instance.Print(this);
		}

		public override bool Equals(object obj)
		{
			if( obj is Value)
			{
				return this.Equals((Value)obj);
			}
			else
			{
				return false;
			}
		}

		[MethodImpl(MethodImplOptions.AggressiveInlining)]
		public bool Equals(Value x)
		{
			if (x.val_ == val_)
			{
				switch (this.ValueType)
				{
					case ValueType.String:
						return (string)this.obj_ == (string)x.obj_;
					case ValueType.Table:
					case ValueType.LispApi:
					case ValueType.Object:
					case ValueType.Symbol:
					case ValueType.Cons:
					case ValueType.ByteVector:
					case ValueType.Vector:
						return this.obj_ == x.obj_;
					default:
						return true;
				}
			}
			else
			{
				if (IsNumber)
				{
					switch (this.ValueType)
					{
						case ValueType.Integer:
							if (x.IsFloat)
							{
								return x.AsFloat == ConvertToFloat();
							}
							else
							{
								return false;
							}
						case ValueType.Float:
							if (x.IsInteger)
							{
								return x.ConvertToFloat() == AsFloat;
							}
							else
							{
								return false;
							}
						default:
							return false;
					}
				}
				else
				{
					return false;
				}
			}
		}

		public override int GetHashCode()
		{
			return ((int)val_) ^ (obj_.GetHashCode());
		}

		[MethodImpl(MethodImplOptions.AggressiveInlining)]
		public static bool operator ==(Value a, Value b)
		{
			return a.Equals(b);
		}

		[MethodImpl(MethodImplOptions.AggressiveInlining)]
		public static bool operator !=(Value a, Value b)
		{
			return !a.Equals(b);
		}

		public bool IsSymbolOrIdentifier
		{
			[MethodImpl(MethodImplOptions.AggressiveInlining)]
			get
			{
				return ValueType == ValueType.Symbol || ValueType == ValueType.Identifier;
			}
		}

		[MethodImpl(MethodImplOptions.AggressiveInlining)]
		public Symbol GetResolvedSymbol()
		{
			if (ValueType == ValueType.Symbol)
			{
				return AsSymbol;
			}
			else
			{
				return AsIdentifier.Symbol;
			}
		}

	}
}
