using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace Lisp.Stdlib
{
	class PortLib
	{
		[LispApi("port?")]
		public static Value port_p(Context ctx, Value v)
		{
			return new Value(v.Is<Port>());
		}

		[LispApi("%open-file")]
		public static Value open_file(Context ctx, Value v, Value output, Value binary)
		{
			// TODO: オプション引数
			var path = v.AsString;
			var isOutput = output.AsBool;
			var isBinary = binary.AsBool;
			var access = isOutput ? FileAccess.Write : FileAccess.Read;
			var stream = File.Open(path, FileMode.OpenOrCreate, access, FileShare.ReadWrite);
			var port = new Port(stream, path);
			return new Value(port);
		}

		[LispApi]
		public static Value delete_file(Context ctx, Value v)
		{
			// TODO: 返り値は？
			File.Delete(v.AsString);
			return C.Nil;
		}

		[LispApi("file-exists?")]
		public static Value file_exist(Context ctx, Value v)
		{
			return new Value(File.Exists(v.AsString));
		}

		[LispApi("binary-port?")]
		public static Value binary_port(Context ctx, Value v)
		{
			var port = v.As<Port>();
			return new Value(port.IsBinary);
		}

		[LispApi("char-ready?")]
		public static Value char_ready(Context ctx, Value v)
		{
			var port = v.As<Port>();
			return new Value(port. IsReady);
		}

		[LispApi("u8-ready?")]
		public static Value u8_ready(Context ctx, Value v)
		{
			var port = v.As<Port>();
			return new Value(port.IsReady);
		}

		[LispApi("%close")]
		public static Value close(Context ctx, Value v)
		{
			var port = v.As<Port>();
			port.Dispose();
			return C.Nil;
		}

		[LispApi("eof-object?")]
		public static Value eof_object_p(Context ctx, Value v)
		{
			return new Value(v == C.Eof);
		}

		[LispApi]
		public static Value eof_object(Context ctx)
		{
			return C.Eof;
		}

		[LispApi]
		public static Value flush_output_port(Context ctx, Value v)
		{
			var port = v.As<Port>();
			port.Flush();
			return C.Nil;
		}

		[LispApi]
		public static Value get_output_bytevector(Context ctx, Value v)
		{
			var port = v.As<Port>();
			var stream = port.RawStream as MemoryStream;
			if (stream != null)
			{
				port.Flush();
				return new Value(stream.ToArray());
			}
			else
			{
				throw new Exception("Invalid port type");
			}
		}

		[LispApi("%open?")]
		public static Value open(Context ctx, Value v)
		{
			var port = v.As<Port>();
			return new Value(port.IsOpen);
		}

		[LispApi("input-port?")]
		public static Value input_port(Context ctx, Value v)
		{
			var port = v.As<Port>();
			return new Value(port.IsInputPort);
		}

		[LispApi("output-port?")]
		public static Value output_port(Context ctx, Value v)
		{
			var port = v.As<Port>();
			return new Value(port.IsOutputPort);
		}

		[LispApi]
		public static Value peek_char(Context ctx, Value v)
		{
			var port = v.As<Port>();
			var c = port.PeekChar();
			if (c == -1)
			{
				return C.Eof;
			}
			else
			{
				return new Value((char)c);
			}
		}

		[LispApi]
		public static Value peek_u8(Context ctx, Value v)
		{
			// TODO: charを使ってる
			var port = v.As<Port>();
			var c = port.PeekChar();
			if (c == -1)
			{
				return C.Eof;
			}
			else
			{
				return new Value((byte)c);
			}
		}

		[LispApi]
		public static Value read_char(Context ctx, Value v)
		{
			var port = v.As<Port>();
			var c = port.ReadChar();
			if (c == -1)
			{
				return C.Eof;
			}
			else
			{
				return new Value((char)c);
			}
		}

		[LispApi]
		public static Value read_u8(Context ctx, Value v)
		{
			var port = v.As<Port>();
			var c = port.ReadChar();
			if (c == -1)
			{
				return C.Eof;
			}
			else
			{
				return new Value((byte)c);
			}
		}

		[LispApi]
		public static Value write_char(Context ctx, Value v, Value c)
		{
			var port = v.As<Port>();
			port.WriteChar(c.AsChar);
			return C.Nil;
		}

		[LispApi]
		public static Value write_u8(Context ctx, Value v, Value c)
		{
			var port = v.As<Port>();
			port.WriteChar((char)c.AsInt);
			return C.Nil;
		}

		[LispApi]
		public static Value open_input_bytevector(Context ctx, Value v)
		{
			var stream = new MemoryStream(v.AsByteVector);
			return new Value(new Port(stream));
		}

		[LispApi]
		public static Value open_output_bytevector(Context ctx)
		{
			var stream = new MemoryStream();
			return new Value(new Port(stream));
		}

		[LispApi]
		public static Value current_input_port(Context ctx)
		{
			return new Value(new Port(Console.OpenStandardInput()));
		}

		[LispApi]
		public static Value current_output_port(Context ctx)
		{
			return new Value(new Port(Console.OpenStandardOutput()));
		}

		[LispApi]
		public static Value current_error_port(Context ctx)
		{
			return new Value(new Port(Console.OpenStandardError()));
		}

		[LispApi]
		public static Value read_bytevector(Context ctx, Value n, Value port)
		{
			// TODO: 引数１個の場合
			// TODO: eofの扱い
			var buf = new byte[n.AsInt];
			var len = port.As<Port>().Read(buf, 0, buf.Length);
			if( len != buf.Length)
			{
				var newBuf = new byte[len];
				buf.CopyTo(newBuf, len);
				buf = newBuf;
			}
			return new Value(buf);
		}

		[LispApi("read-bytevector!")]
		public static Value read_bytevector_e(Context ctx, Value[] args)
		{
			// TODO: 引数１個の場合
			// TODO: eofの扱い
			var bv = args[0].AsByteVector;
			var port = args[1].As<Port>();
			var start = (args.Length > 2) ? args[2].AsInt : 0;
			var end = (args.Length > 2) ? args[2].AsInt : bv.Length;

			return new Value(port.Read(bv, start, end));
		}

		[LispApi]
		public static Value read_line(Context ctx, Value[] args)
		{
			// TODO: 引数１個の場合
			// TODO: eofの扱い
			var port = args[0].As<Port>();
			var line = port.RawReader.ReadLine();
			if (line == null)
			{
				return C.Eof;
			}
			else
			{
				return new Value(line);
			}
		}

		[LispApi]
		public static Value read_string(Context ctx, Value[] args)
		{
			// TODO: 引数１個の場合
			// TODO: eofの扱い
			var n = args[0].AsInt;
			var port = args[1].As<Port>();

			var buf = new char[n];

			var len = port.RawReader.ReadBlock(buf, 0, buf.Length);

			if (len == 0)
			{
				return C.Eof;
			}
			else
			{
				return new Value(new string(buf, 0, len));
			}
		}


		[LispApi]
		public static Value write_bytevector(Context ctx, Value[] args)
		{
			// TODO: 引数１個の場合
			// TODO: eofの扱い
			var bv = args[0].AsByteVector;
			var port = args[1].As<Port>();
			var start = (args.Length > 2) ? args[2].AsInt : 0;
			var end = (args.Length > 2) ? args[2].AsInt : bv.Length;

			port.Write(bv, start, end);

			return C.Nil;
		}

		[LispApi]
		public static Value write_string(Context ctx, Value[] args)
		{
			// TODO: 引数１個の場合
			// TODO: eofの扱い
			var str = args[0].AsString;
			var port = args[1].As<Port>();
			var start = (args.Length > 2) ? args[2].AsInt : 0;
			var end = (args.Length > 2) ? args[2].AsInt : str.Length;

			port.RawWriter.Write(str.Substring(start, end - start));

			return C.Nil;
		}

	}
}
