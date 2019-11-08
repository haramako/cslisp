using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;

namespace Lisp
{
    class Context
    {
        public Value bundle_get(Symbol symbol)
        {
            return C.Nil;
        }

    }

    class Eval
    {
        bool opt_trace;

        Value _eval_direct(Context ctx, Value code)
        {
            switch (code.ValueType)
            {
                case ValueType.Nil:
                case ValueType.Integer:
                case ValueType.Float:
                case ValueType.Bool:
                case ValueType.String:
                case ValueType.Object:
                    return code;
                case ValueType.Symbol:
                    return ctx.bundle_get(code.AsSymbol);
                default:
                    throw new Exception("BUG");
            }
        }

        string v2s(Value v, int limit = 100)
        {
            return PrettyPrinter.Instance.Print(v, limit);
        }

        void trace(string format, params object[] param)
        {
            Console.WriteLine(format, param);
        }

        Value eval_loop(Context ctx, Port stream)
        {
            Value result = C.Nil;
            for (; ; )
            {
                Value stat = stream.ReadValue();
                if (opt_trace) trace("trace: {0}", v2s(stat));
                if (stat == C.Eof) break;

                stat = normalize_sexp(ctx, stat);
                result = eval(ctx, stat);
            }
            return result;
        }

        void ERROR(string msg)
        {

        }

        Value eval(Context ctx, Value sexp)
        {
            //printf( "eval: %s\n", v2s(sexp));

            Value result = C.Nil;
            Value code = C.Nil; // TODO
            Value ret = C.Nil; // TODO
            var stack = new List<Value>();
            Value cont = CONT(sexp, ctx->bundle, C.Nil);
            String src_filename = null;
            int src_line = 0;

        _loop:

            if (cont == C.Nil)
            {
                return result;
            }
            //printf( "> %s => %s\n", v2sn(result,20), v2sn(C_CODE(cont), 80) );

#if false
            if (IS_PAIR_SOURCE(C_CODE(cont)))
            {
                PairSource src = V2PAIR_SOURCE(C_CODE(cont));
                src_filename = src->filename;
                src_line = src->line;
                //printf( "> %s at %s:%d\n", v2sn(C_CODE(cont), 20), v2s((Value)(src->filename)), src->line);
            }
#endif


            switch (code.ValueType)
            {
                case ValueType.Nil:
                case ValueType.Integer:
                case ValueType.Float:
                case ValueType.Bool:
                case ValueType.String:
                case ValueType.Object:
                    {
                        Value v = _eval_direct(ctx, code);
                        if (v == null) {
                            ERROR("symbol '{0}' not found", v2s(code));
                        }
                        code = next;
                        stack.Add(v);
                        result = v;
                    }
                case TYPE_PAIR:
                case TYPE_PAIR_SOURCE:
                    if (!IS_SPECIAL(CAR(C_CODE(cont))))
                    {
                        NEXT_DIRECT(CAR(C_CODE(cont)),
                                     CONT_OP(V_APP, cons(CDR(C_CODE(cont)), C.Nil), C_BUNDLE(cont), C_NEXT(cont)));
                    }

                    Value code = CDR(C_CODE(cont));
                    Operator op = V2SPECIAL(CAR(C_CODE(cont)))->op;
                    switch (op)
                    {
                        case OP_BEGIN:
                            // display_val( "OP_BEGIN: ", code );
                            if (code == C.Nil)
                            {
                                NEXT(C_NEXT(cont), result);
                            }
                            else if (CDR(code) == C.Nil)
                            {
                                NEXT_DIRECT(CAR(code), C_NEXT(cont));
                            }
                            else
                            {
                                NEXT_DIRECT(CAR(code),
                                             CONT_OP(V_BEGIN, CDR(code), C_BUNDLE(cont), C_NEXT(cont)));
                            }

                        case OP_QUOTE:
                            NEXT(C_NEXT(cont), CAR(code));

                        case OP_APP:
                            {
                                //printf( "OP_APP: %s %s\n", v2s_limit(code,100), v2s_limit(result,10) );
                                Value rest, tmp;
                                bind2cdr(code, rest, tmp);
                                tmp = cons(result, tmp);
                                if (rest != C.Nil)
                                {
                                    NEXT_DIRECT(CAR(rest), CONT_OP(V_APP, cons(CDR(rest), tmp), C_BUNDLE(cont), C_NEXT(cont)));
                                }
                                else
                                {
                                    Value vals = C.Nil;
                                    LIST_EACH(it, tmp) {
                                        vals = cons(it, vals);
                                    }

                                    Value lmd = CAR(vals);
                                    if (IS_LAMBDA(lmd) || IS_CFUNC(lmd))
                                    {
                                        Value val = C.Nil;
                                        Value next = call(ctx, lmd, CDR(vals), cont, &val);
                                        NEXT(next, val);
                                    }
                                    else if (IS_CONTINUATION(lmd))
                                    {
                                        vals = CDR(vals);
                                        if (CDR(vals) != C.Nil)
                                        {
                                            NEXT(lmd, cons(V(SYM_VALUES), vals));
                                        }
                                        else
                                        {
                                            NEXT(lmd, CAR(vals));
                                        }
                                    }
                                    else
                                    {
                                        assert(0);
                                    }
                                }
                            }

                        case OP_DEFINE:
                            NEXT_DIRECT(CADR(code),
                                        CONT_OP(V_DEFINE2, CAR(code), C_BUNDLE(cont), C_NEXT(cont)));

                        case OP_DEFINE2:
                            bundle_define(C_BUNDLE(cont), V2SYMBOL(code), result);
                            NEXT(C_NEXT(cont), C.Nil);

                        case OP_SET_I:
                            NEXT_DIRECT(CADR(code),
                                         CONT_OP(V_SET_I2, CAR(code), C_BUNDLE(cont), C_NEXT(cont)));

                        case OP_SET_I2:
                            bundle_set(C_BUNDLE(cont), V2SYMBOL(code), result);
                            NEXT(C_NEXT(cont), C.Nil);

                        case OP_LAMBDA:
                        case OP_MACRO:
                            {
                                // display_val( "lambda2:", result );
                                Lambda lmd = lambda_new();
                                lmd->type = (op == OP_LAMBDA) ? LAMBDA_TYPE_LAMBDA : LAMBDA_TYPE_MACRO;
                                lmd->bundle = C_BUNDLE(cont);
                                lmd->name = NULL;
                                lmd->args = CAR(code);
                                lmd->body = CDR(code);
                                NEXT(C_NEXT(cont), V(lmd));
                            }

                        case OP_DEFINE_SYNTAX2:
                            //printf("SYNTAX2: %s\n", v2s(code));
                            NEXT_DIRECT(CADR(code),
                                         CONT_OP(V_DEFINE_SYNTAX22, CAR(code), C_BUNDLE(cont), C_NEXT(cont)));

                        case OP_DEFINE_SYNTAX22:
                            {
                                //printf("SYNTAX22: %s %s\n", v2s(code), v2s(result));
                                Lambda lmd = V2LAMBDA(result);
                                lmd->type = LAMBDA_TYPE_MACRO;
                                bundle_define(C_BUNDLE(cont), V2SYMBOL(code), result);
                                NEXT(C_NEXT(cont), C.Nil);
                            }

                        case OP_IF:
                            NEXT_DIRECT(CAR(code),
                                         CONT_OP(V_IF2, CDR(code), C_BUNDLE(cont), C_NEXT(cont)));

                        case OP_IF2:
                            if (result != VALUE_F)
                            {
                                NEXT_DIRECT(CAR(code), C_NEXT(cont));
                            }
                            else
                            {
                                NEXT(CONT_OP(V_BEGIN, CDR(code), C_BUNDLE(cont), C_NEXT(cont)), C.Nil);
                            }
                    }
            }
            Debug.Assert(false);
        }

        Value normalize_sexp(Context ctx, Value s)
        {
            // printf( "s:%s\n", v2s_limit(s,30) );
            if (!IS_PAIR(s)) return s;
            if (IS_PAIR(CAR(s))) return normalize_list(ctx, s);
            if (TYPE_OF(CAR(s)) != TYPE_SYMBOL) return s;

            Symbol sym = V2SYMBOL(CAR(s));
            Value rest = CDR(s);
            if (sym == SYM_DEFINE)
            {
                if (IS_SYMBOL(CAR(rest)))
                {
                    // (define sym val) の形
                    return cons_src(s, V_DEFINE, normalize_list(ctx, rest));
                }
                else if (IS_PAIR(CAR(rest)))
                {
                    // (define (sym args ...) ... ) の形
                    Value lambda = cons_src(s, V_LAMBDA,
                                             cons_src(CAR(rest), CDAR(rest), normalize_list(ctx, CDR(rest))));
                    return cons_src(s, V_DEFINE,
                                     cons_src(CAR(rest), CAAR(rest),
                                              cons_src(CDR(rest),
                                                        lambda, C.Nil)));
                }
                else
                {
                    assert(0);
                }
            }
            else if (sym == SYM_LAMBDA)
            {
                return cons3(V_LAMBDA, CAR(rest), normalize_list(ctx, CDR(rest)));
            }
            else if (sym == SYM_DEFINE_SYNTAX2)
            {
                return cons_src(s, V_DEFINE_SYNTAX2, normalize_list(ctx, rest));

            }
            else if (sym == SYM_IF)
            {
                Value _cond, _then, _else;
                bind3cdr(rest, _cond, _then, _else);
                if (_else == C.Nil) _else = cons(V_UNDEF, C.Nil);
                return cons4(V_IF, normalize_sexp(ctx, _cond), normalize_sexp(ctx, _then), normalize_list(ctx, _else));

            }
            else if (sym == SYM_BEGIN)
            {
                if (rest == C.Nil) return V_UNDEF;
                if (CDR(rest) == C.Nil) return normalize_sexp(ctx, CAR(rest));
                return cons(V_BEGIN, normalize_list(ctx, rest));

            }
            else if (sym == SYM_SET_I)
            {
                return cons3(V_SET_I, CAR(rest), normalize_sexp(ctx, CDR(rest)));

            }
            else if (sym == SYM_QUOTE)
            {
                return cons(V_QUOTE, rest);

            }
            else
            {
                s = normalize_syntax(ctx, s);
                return normalize_list(ctx, s);
            }
        }

        // implicit begin
        Value normalize_begin(Context ctx, Value list)
        {
            return normalize_sexp(ctx, cons_src(list, (Value)SYM_BEGIN, list));
        }

        Value normalize_list(Context ctx, Value list)
        {
            if (IS_PAIR(list))
            {
                return cons_src(list, normalize_sexp(ctx, CAR(list)), normalize_list(ctx, CDR(list)));
            }
            else
            {
                return list;
            }
        }

        Value normalize_syntax(Context ctx, Value s)
        {
            if (!IS_SYMBOL(CAR(s))) return s;

            //printf("hoge: %s\n", v2s(bundle_get( ctx->bundle, intern("define-syntax"), C.Nil )));
            Value v = bundle_get(ctx->bundle, V2SYMBOL(CAR(s)), C.Nil);
            if (!IS_LAMBDA(v)) return s;

            Lambda lmd = V2LAMBDA(v);
            if (lmd->type != LAMBDA_TYPE_MACRO)
            {
                return s;
            }

            //printf("normalize_syntax: %s\n", v2s(s));

            s = eval(ctx, cons5(v, cons(V_QUOTE, cons(s, C.Nil)), C.Nil, C.Nil, C.Nil));

            //printf("normalize_syntax2: %s\n", v2s(s));

            return normalize_sexp(ctx, s);
        }
    }
}
