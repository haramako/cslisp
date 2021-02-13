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
	}
}
