using System;
using System.Collections.Generic;
using System.Text;

namespace Lisp
{
    public class Symbol
    {
        static Dictionary<string, Symbol> symbolDict = new Dictionary<string, Symbol>();
        static Dictionary<int, Symbol> symbolIdDict = new Dictionary<int, Symbol>();

        static int nextId = 1;

        int id_;

        string ident_;


        Symbol(string ident)
        {
            id_ = nextId++;
            ident_ = ident;
        }

        public override string ToString()
        {
            return ident_;
        }

        static public Symbol Intern(string ident)
        {
            Symbol found;
            if( symbolDict.TryGetValue(ident, out found) ){

                return found;
            }
            else
            {
                var symbol = new Symbol(ident);
                symbolDict.Add(ident, symbol);
                symbolIdDict.Add(symbol.id_, symbol);
                return symbol;
            }
        }

        static public Symbol FindById(int id)
        {
            Symbol found;
            if (symbolIdDict.TryGetValue(id, out found))
            {

                return found;
            }
            else
            {
                throw new Exception($"symbol id {id} not found");
            }
        }
    }
}
