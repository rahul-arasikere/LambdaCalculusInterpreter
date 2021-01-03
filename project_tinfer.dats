#include "share/atspre_staload.hats"

#staload "./project.sats"
#staload "./mylib.sats"


implement t1dcl_tinfer(t1dcl) =
(
    case t1dcl of
    | mylist_cons(hd, tl) => 
    (
        let
            val T1DCL(va0,t1m0)=hd
            val t1vtp = va0.type()
            val t1p = t1erm_tinfer(t1m0)
            val-true=type1_unify(t1vtp,t1p)
        in
            t1dcl_tinfer(tl)
        end

    )
    | mylist_nil() => () 
)

local
    fun recursive_type_fetch(t1p0: type1): type1 =
    (
        case t1p0 of
        | T1Pext(t1pext0) => 
        (
            case t1pext0.get() of
            | myoptn_cons(t1p1) => recursive_type_fetch(t1p1)
            | myoptn_nil() => t1p0
        )
        | _ => t1p0
    )
    fun auxvar(t1m0: t1erm): type1 =
    let
        val-T1Mvar(t1v) = t1m0.node() in t1v.type()
    end

    fun auxfst(t1m0: t1erm): type1 =
    let
        val-T1Mfst(tup1) = t1m0.node()
        val t1p1 = t1erm_tinfer(tup1)
        val-true = type1_unify(t1p1, type1_new_ext())
        val-T1Ptup(tfst, _) = t1p1 
    in 
        tfst
    end

    fun auxsnd(t1m0: t1erm): type1 =
    let
        val-T1Msnd(tup1) = t1m0.node()
        val t1p1 = t1erm_tinfer(tup1)
        val-true = type1_unify(t1p1, type1_new_ext())
        val-T1Ptup(_, tsnd) = t1p1
    in
        tsnd
    end
in
    implement t1erm_tinfer(t1) =
    (
        case t1.node() of
        | T1Mnil _ => T1Pnil
        | T1Mint _ => T1Pint
        | T1Mbtf _ => T1Pbool
        | T1Mstr _ => T1Pstring
        | T1Mvar _ => auxvar(t1)
        | T1Mlam(va0, t1pr, t1m0) =>
        (
            let
                val t1pt1m0=t1erm_tinfer(t1m0)
                val-true=type1_unify(t1pt1m0,t1pr)
                val t1pr2=recursive_type_fetch(t1pr)
                val t1pa=recursive_type_fetch(va0.type())
            in
                T1Pfun(t1pa,t1pr2)
            end
        )
        | T1Mfix(va0, t1p0, t1m0) => 
        (
            let
                val tpt1m0 = t1erm_tinfer(t1m0)
                val-true = type1_unify(t1p0, tpt1m0)
            in
                recursive_type_fetch(t1p0)
            end
        )
        | T1Mlet(tdcl, t1m0) =>
        (
            let
                val () = t1dcl_tinfer(tdcl)
            in
                t1erm_tinfer(t1m0)
            end
        )
        | T1Mfst _ => auxfst(t1)
        | T1Msnd _ => auxsnd(t1)
        | T1Mtup(fst, snd) =>
        (
            T1Ptup(t1erm_tinfer(fst), t1erm_tinfer(snd))
        )
        | T1Mapp(t1m0, t1m1) =>
        (
            let
                val t1p1 = t1erm_tinfer(t1m0)
                val t1p2 = t1erm_tinfer(t1m1)
            in
            case+ t1p1 of
            | T1Pext(tex) =>
                let
                    val targ=T1Pext(tpext_new())
                    val tres=T1Pext(tpext_new())
                    val-true = type1_unify(T1Pfun(targ,tres),t1p1)
                in
                    tres
                end
            | _ =>
                let
                    val-T1Pfun(targ, tres)=t1p1
                    val-true = type1_unify(targ, t1p2)
                in
                    tres
                end
            end
        )
        | T1Mopr1(opr, t1m0) =>
        (
            let
                val t1p0 = t1erm_tinfer(t1m0)
            in
                case+ opr of
                | "print" => 
                (
                    case- t1p0 of
                    | T1Pbas(nm1)
                        when nm1 = "nil" => T1Pnil
                    | T1Pbas(nm1)
                        when nm1 = "int" => T1Pnil
                    | T1Pbas(nm1)
                        when nm1 = "bool" => T1Pnil
                    | T1Pbas(nm1)
                        when nm1 = "string" => T1Pnil
                )
                | _ => t1p0
            end
        )
        | T1Mopr2(opr, t1m0, t1m1) =>
        (
            let
                val t1p0 = t1erm_tinfer(t1m0)
                val t1p1 = t1erm_tinfer(t1m1)
                val-true = (type1_unify(t1p0,T1Pint)||type1_unify(t1p0, T1Pbool))
                val-true = (type1_unify(t1p1,T1Pint)||type1_unify(t1p1, T1Pbool))
            in
                case+ opr of
                | ">" => T1Pbool
                | "<" => T1Pbool
                | ">=" => T1Pbool
                | "<=" => T1Pbool
                | "==" => T1Pbool
                | "!=" => T1Pbool
                | _ => t1p0
            end
        )
        | T1Manno(t1m0, t1p0) =>
        (
            let
                val t1p1 = t1erm_tinfer(t1m0)
                val-true = type1_unify(t1p0, t1p1)
            in
                t1p0
            end
        )
        | T1Mcond(t1m0, t1m1, t1m2) =>
        (
            let
                val t1p0 = t1erm_tinfer(t1m0)
                val t1p1 = t1erm_tinfer(t1m1)
                val t1p2 = t1erm_tinfer(t1m2)
                val-true = type1_unify(T1Pbool, t1p0)
                val-true = type1_unify(t1p1, t1p2)
            in
                t1p1
            end
        )
    )
end

implement t1pgm_tinfer(tpgm) = 
(
    let
        val-T1PGM(t1dcl, t1m0) = tpgm
        val () = t1dcl_tinfer(t1dcl)
    in
        t1erm_tinfer(t1m0)
    end
)

(* ****** ****** *)

(* end of [project_tinfer.dats] *)
