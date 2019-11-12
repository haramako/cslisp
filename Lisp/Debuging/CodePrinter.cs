using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace Lisp.Debug
{
	public class CodePrinter
	{
		public CodePrinter()
		{

		}

		public void PrintLambda(TextWriter w, Lambda lmd)
		{
			w.WriteLine("========== Lambda ============");
			if (lmd.Name != null)
			{
				w.WriteLine($"Name: {lmd.Name} ({lmd.DefinedLocation.DisplayString})");
			}
			else
			{
				w.WriteLine($"Name: #{lmd.Id} ({lmd.DefinedLocation.DisplayString})");
			}

			w.WriteLine($"Original Source: {lmd.OriginalSource}");
			w.WriteLine($"Expanded Source: {lmd.ExpandedSource}");

			w.WriteLine("Code:");
			var code = lmd.Code;
			for (int i = 0; i < code.Length; i++)
			{
				var c = code[i];
				var l = lmd.Locations[i];
				w.WriteLine("{0:0000} {1,-6} {2,-30}   {3}", i, c.Op, c.Val, l.DisplayString);
			}

			w.Flush();
		}
	}
}
