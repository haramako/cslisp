using System;
using System.Collections.Generic;

namespace Lisp
{
    public class Lambda
    {
        public Code[] Code;
        public Symbol[] Params;
        public Symbol RestParam;

        // for debug
        public Symbol Name;
        public SourceLocation DefinedLocation;
        public SourceLocation[] Locations;

        public Lambda(Value param, Code[] codes, SourceLocation[] locations)
        {
            Code = codes;
            Locations = locations;

            var paramList = new List<Symbol>();
            for (var p = param; !p.IsNil; p = p.Cdr)
            {
                if (p.IsCons)
                {
                    paramList.Add(p.Car.AsSymbol);
                }
                else
                {
                    // rest parameter
                    RestParam = p.AsSymbol;
                    break;
                }
            }
            Params = paramList.ToArray();
        }

        public override string ToString()
        {
            if (Name != null)
            {
                return $"#<lambda {Name}>";
            }
            else
            {
                return $"#<lambda {DefinedLocation.Filename}:{DefinedLocation.Line}>";
            }
        }
    }
}
