#include "share/atspre_staload.hats"
#include "share/atspre_staload_libats_ML.hats"

#staload "./project.sats"
#staload "./mylib.sats"

local
    fun redefine_t2val(val1: t2val): string =
    (
        case- val1 of
        | T2Vnil() => "NULL"
        | T2Vint(i0) => "LAMVAL_int("+strptr2string(g0int2string(i0))+")" 
        | T2Vstr(s0) => "LAMVAL_str(\""+s0+"\")"
        | T2Vbtf(b0) => if b0 then "LAMVAL_btf(true)" else "LAMVAL_btf(false)"
        | T2Vtmp(t0) => t0
        | T2Vfun(f0) => f0
    )
    fun fprint_t2inslst(out: FILEref, t2m1: t2inslst): void =
    (
        case- t2m1 of
        | mylist_cons(hd, tl) => 
        (
            let
                val () = t2ins_emit(out, hd)
            in
                fprint_t2inslst(out, tl)
            end
        )
        | mylist_nil() => ()
    )
    fun fprint_tvalist(out: FILEref, tvalst:t2valist, i0:int): void =
    (
        case- tvalst of
        | mylist_cons(hd, tl) =>
        (
            case- hd of
            | T2Vtmp(t0) =>
            (
                let
                    val () = fprint!(out, "lamval ", t0, " = LAMVAL_get(args, ", i0, ");\n")
                in
                    fprint_tvalist(out, tl, i0+1)
                end
            )
            // | _ => fprint_tvalist(tl)
        )
        | mylist_nil() => ()
    )
    fun emit_top_level_globals(out: FILEref, dcl: mylist(string)): void = 
    (
        case- dcl of
        | mylist_cons(hd, tl) =>
        (
            let
                val () = emit_top_level_globals(out, tl) 
            in
                fprint!(out, "lamval ", hd, ";\n")
            end
        )
        | mylist_nil() => ()
    )
in
    implement t2ins_emit(out, t2m0) =
    (
        case- t2m0 of
        | T2INSmov(tmp, val1) => 
        (
            fprint!(out, tmp, "=", redefine_t2val(val1), ";\n")
        )
        | T2INSclo(tmp, fname, tvalst, tllst) => 
        (
            let
                val () = fprint!(out, "lamval ", fname, "(lamval args)\n")
                val () = fprint!(out, "{\n")
                val () = fprint_tvalist(out, tvalst, 0)
                val () = fprint!(out, "lamval ", tmp, ";\n")
                val () = fprint_t2inslst(out, tllst)
                val () = fprint!(out, "return ", tmp, ";\n")
            in
                fprint!(out, "}\n\n")
            end
        )
        | T2INSpos(tmp, val1) => (fprint!(out, "lamval ", tmp, " = ", redefine_t2val(val1), ";\n"))
        | T2INSsub(tmp, val1) => (fprint!(out, "lamval ", tmp, " = ", "LAMOPR_isub( LAMVAL_int(0), ", redefine_t2val(val1), ");\n"))
        | T2INSneg(tmp, val1) => (fprint!(out, "lamval ", tmp, " = ", "LAMOPR_neg(", redefine_t2val(val1), ");\n"))
        | T2INSprint(tmp, val1) => (fprint!(out, "LAMVAL_print(", redefine_t2val(val1), ");\n"))
        | T2INSiadd(tmp, val1, val2) => (fprint!(out, "lamval ", tmp, " = ", "LAMOPR_iadd(", redefine_t2val(val1), ",", redefine_t2val(val2), ");\n"))
        | T2INSisub(tmp, val1, val2) => (fprint!(out, "lamval ", tmp, " = ", "LAMOPR_isub(", redefine_t2val(val1), ",", redefine_t2val(val2), ");\n"))
        | T2INSimul(tmp, val1, val2) => (fprint!(out, "lamval ", tmp, " = ", "LAMOPR_imul(", redefine_t2val(val1), ",", redefine_t2val(val2), ");\n"))
        | T2INSidiv(tmp, val1, val2) => (fprint!(out, "lamval ", tmp, " = ", "LAMOPR_idiv(", redefine_t2val(val1), ",", redefine_t2val(val2), ");\n"))
        | T2INSimod(tmp, val1, val2) => (fprint!(out, "lamval ", tmp, " = ", "LAMOPR_imod(", redefine_t2val(val1), ",", redefine_t2val(val2), ");\n"))
        | T2INSneq(tmp, val1, val2) => (fprint!(out, "lamval ", tmp, " = ", "LAMOPR_ineq(", redefine_t2val(val1), ",", redefine_t2val(val2), ");\n"))
        | T2INSeq(tmp, val1, val2) => (fprint!(out, "lamval ", tmp, " = ", "LAMOPR_ieq(", redefine_t2val(val1), ",", redefine_t2val(val2), ");\n"))
        | T2INSlt(tmp, val1, val2) => (fprint!(out, "lamval ", tmp, " = ", "LAMOPR_ilt(", redefine_t2val(val1), ",", redefine_t2val(val2), ");\n"))
        | T2INSlte(tmp, val1, val2) => (fprint!(out, "lamval ", tmp, " = ", "LAMOPR_ilte(", redefine_t2val(val1), ",", redefine_t2val(val2), ");\n"))
        | T2INSgt(tmp, val1, val2) => (fprint!(out, "lamval ", tmp, " = ", "LAMOPR_igt(", redefine_t2val(val1), ",", redefine_t2val(val2), ");\n"))
        | T2INSgte(tmp, val1, val2) => (fprint!(out, "lamval ", tmp, " = ", "LAMOPR_igte(", redefine_t2val(val1), ",", redefine_t2val(val2), ");\n"))
        | T2INStup(tmp, val1, val2) => (fprint!(out, "lamval ", tmp, " = ", "LAMVAL_clo(", redefine_t2val(val1), ",", redefine_t2val(val2), ");\n"))
        | T2INSfst(tmp, val1) => (fprint!(out, "lamval ", tmp, " = ", "LAMVAL_get(", redefine_t2val(val1), ", 0);\n"))
        | T2INSsnd(tmp, val1) => (fprint!(out, "lamval ", tmp, " = ", "LAMVAL_get(", redefine_t2val(val1), ", 1);\n"))
        | T2INScall(tmp, val1(*fun*), val2(*arg*)) => 
        (
            fprint!(out, "lamval ", tmp, " = ", "LAMVAL_call(", redefine_t2val(val1), ", ", redefine_t2val(val2), ");\n")
        )
        | T2INScond(tmp, val1(*cond*), t2lst1(*then*), t2lst2(*else*)) => 
        (
            let
                val () = fprint!(out, "lamval ", tmp, ";\n")
                val () = fprint!(out, "if( ((lamval_int)", redefine_t2val(val1), ")->data )\n{\n")
                val () = fprint_t2inslst(out, t2lst1)
                val () = fprint!(out, "}\nelse\n{\n")
                val () = fprint_t2inslst(out, t2lst2)
            in
                fprint!(out, "}\n")
            end
        )
    )

    implement emitter(out, tpgm0) =
    (
        let
            val-T2PGM(tinslst, val1, dcl, tinsdcl) = tpgm0
            val () = fprint!(out, "// This program is generated.\n// Rahul Arasikere.\n\n")
            val () = fprint!(out, "#include \"runtime.h\"\n\n\n")
            val () = emit_top_level_globals(out, dcl)
            fun recursive_emit(tm0: t2inslst): void =
            (
                case- tm0 of
                | mylist_cons(hd, tl) => 
                (
                    case- tl of
                    | mylist_cons(hd1, tl1) =>
                    (
                        let
                            val () = t2ins_emit(out, hd)
                        in
                            recursive_emit(tl)
                        end
                    )
                    | mylist_nil() => 
                    (
                        let
                            val () = fprint!(out, "int main()\n")
                            val () = fprint!(out, "{\n")
                            val () = fprint_t2inslst(out, tinsdcl)
                            val () = t2ins_emit(out, hd)
                            val () = fprint!(out, "LAMVAL_print(", redefine_t2val(val1), ");\n")
                            val () = fprint!(out, "printf(\"\\n\");\n")  
                        in
                            fprint!(out, "}\n")
                        end
                    )
                )
                | mylist_nil() => ()
            )
        in
            recursive_emit(tinslst)
        end
    )
end