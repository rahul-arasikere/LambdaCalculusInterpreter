#include "share/atspre_staload.hats"

#staload "./project.sats"
#staload "./mylib.sats"


implement print_value(x0) = fprint_value(stdout_ref, x0)

(* ****** ****** *)

implement fprint_value(out, x0) =
(
    case+ x0 of
    | VALnil() => fprint!(out, "VALnil(", ")")
    | VALint(i0) => fprint!(out, "VALint(", i0, ")")
    | VALbtf(b0) => fprint!(out, "VALbtf(", b0, ")")
    | VALstr(s0) => fprint!(out, "VALstr(", s0, ")")
    | VALtup(v1, v2) => fprint!(out, "VALtup(", v1, "; ", v2, ")")
    | VALlam(tlam, elam) => fprint!(out, "VALlam(", tlam, "; ... ", ")")
    | VALfix(f0, vlam) => fprint!(out, "VALfix(", f0, "; ", vlam, ")")
)



extern fun t1erm_interp1 (t1m0:t1erm, env0:d1env):value
extern fun tdclist_interp1 (t1dlc:t1dclist,env0:d1env):d1env
extern fun tdclist_interp (t1dlc:t1dclist):d1env

local
    fun aux_var(t1m0: t1erm, env0: d1env): value =
    let
        val-T1Mvar(x0) = t1m0.node()
        val opt = d1env_search(env0, x0)
    in
        case- opt of 
        | myoptn_cons(v0) => v0
        | myoptn_nil() => 
        let
            val () = println!(t1m0)
        in
            VALnil()
        end
    end // end of [aux_var]

    fun aux_app(t1m0: t1erm, env0: d1env): value =
    let
        val-T1Mapp(tfun, targ) = t1m0.node()
        val vfun = t1erm_interp1(tfun, env0)
        val varg = t1erm_interp1(targ, env0)
    in
        case- vfun of
        | VALlam(tlam, elam) =>
        (
            let
                val- T1Mlam(x0,_, body) = tlam.node()
            in
                t1erm_interp1(body, d1env_extend(elam, x0, varg))
            end
        )
        | VALfix(f0, vlam) =>
        (
            let
                val-VALlam(t0, elam) = vlam
                val-T1Mlam(x0,_, body) = t0.node()
                val efix =d1env_extend(elam, f0, vfun)
            in
                t1erm_interp1(body, d1env_extend(efix, x0, varg))
            end
        )

    end // end of [aux_app]

    (* ****** ****** *)

    fun aux_cond(t1m0: t1erm, env0: d1env): value =
    let
        val-T1Mcond(t1, t2, t3) = t1m0.node()
        val v1 = t1erm_interp1(t1, env0)
    in
        case- v1 of
        | VALbtf(b1) =>
        (
            if b1
            then t1erm_interp1(t2, env0)
            else t1erm_interp1(t3, env0)
        )
    end(*let*) // end of [aux_cond]

    (* ****** ****** *)

    fun aux_opr1(t1m0: t1erm, env0: d1env): value =
    let
        val-T1Mopr1(opr, t1) = t1m0.node()
        val v1 = t1erm_interp1(t1, env0)
    in
        case+ opr of
        | "-" =>
        (
            case- v1 of
            | VALint(i1) => VALint(0-i1)
        )
        | "+" => v1
        | "~" =>
        (
            case- v1 of
            | VALint(i1) => VALint(~i1)
            | VALbtf(b1) => VALbtf(~b1)
        )
        | "print" =>
        (
            case- v1 of
            | VALint(i1) =>
            let
                val () = print(i1) 
            in
                VALnil()
            end
            | VALbtf(b1) =>
            let
                val () = print(b1) 
            in
                VALnil()
            end
            | VALstr(s1) =>
            let
                val () = print(s1) 
            in
                VALnil()
            end
        )
        | _ (* else *) =>
        let
            val () = println!("Parse error")
            val () = assertloc(false) 
        in
            exit(1)
        end
    end (*let*) // end of [aux_opr2]

    (* ****** ****** *)

    fun aux_opr2(t1m0: t1erm, env0: d1env): value =
    let
        val-T1Mopr2(opr, t1, t2) = t1m0.node()
        val v1 = t1erm_interp1(t1, env0)
        val v2 = t1erm_interp1(t2, env0)
    in
        case+ opr of
        | "+" =>
        let
            val-VALint(i1) = v1
            val-VALint(i2) = v2 
        in
            VALint(i1 + i2)
        end
        | "-" =>
        let
            val-VALint(i1) = v1
            val-VALint(i2) = v2 
        in
            VALint(i1 - i2)
        end
        | "*" =>
        let
            val-VALint(i1) = v1
            val-VALint(i2) = v2 
        in
            VALint(i1 * i2)
        end
        | "/" =>
        let
            val-VALint(i1) = v1
            val-VALint(i2) = v2 
        in
            VALint(i1 / i2)
        end
        | "%" =>
        let
            val-VALint(i1) = v1
            val-VALint(i2) = v2 
        in
            VALint(i1 - (i1/i2)*i2)
        end
        | ">" =>
        let
            val-VALint(i1) = v1
            val-VALint(i2) = v2 
        in
            VALbtf(i1 > i2)
        end
        | "<" =>
        let
            val-VALint(i1) = v1
            val-VALint(i2) = v2 
        in
            VALbtf(i1 < i2)
        end
        | "=" =>
        let
            val-VALint(i1) = v1
            val-VALint(i2) = v2 
        in
            VALbtf(i1 = i2)
        end
        | "==" =>
        let
            val-VALint(i1) = v1
            val-VALint(i2) = v2 
        in
            VALbtf(i1 = i2)
        end
        | "<=" =>
        let
            val-VALint(i1) = v1
            val-VALint(i2) = v2 
        in
            VALbtf(i1 <= i2)
        end
        | ">=" =>
        let
            val-VALint(i1) = v1
            val-VALint(i2) = v2 
        in
            VALbtf(i1 >= i2)
        end
        | "!=" =>
        let
            val-VALint(i1) = v1
            val-VALint(i2) = v2 
        in
            VALbtf(i1 != i2)
        end
        | _ (* else *) =>
        let
            val () = assertloc(false) 
        in
            exit(1)
        end where
        {
            val () = println!("Operator error!")
            val () = println! (t1m0)
        }
    end (*let*) // end of [aux_opr2]

in
    implement t1erm_interp1(t1m0, env0) =
    (
        case- t1m0.node() of
        | T1Mnil() => VALnil()
        | T1Mbtf(i0) => VALbtf(i0)
        | T1Mint(i0) => VALint(i0)
        | T1Mstr(s0) => VALstr(s0)
        | T1Mvar _ => aux_var(t1m0, env0)
        | T1Mlet(tdl0, body) =>
        let
            val new_env=tdclist_interp1(tdl0,env0)
        in
            t1erm_interp1(body,new_env)
        end
        | T1Mapp _ => aux_app(t1m0, env0)
        | T1Mlam _ => VALlam(t1m0, env0)
        | T1Mfix(f0,_, t1m1) =>
        (
            VALfix(f0, vlam)
        ) where 
        {
            val vlam = VALlam(t1m1, env0)
        }
        | T1Mcond _ =>  aux_cond(t1m0, env0)
        | T1Mopr1 _ =>  aux_opr1(t1m0,env0)
        | T1Mopr2 _ =>  aux_opr2(t1m0, env0)
        | T1Mfst(t0) =>
        (
            let
                val tv0=t1erm_interp1(t0,env0)
            in
                case- tv0 of
                | VALtup(t1,t2) => t1
                | _ =>
                let 
                    val () = println!("t1mfst: Error reconciling tuple ,", t0, "; got: ", tv0)
                    val () = assertloc(false)
                in
                    exit(1)
                end
            end
        )
        | T1Msnd(t0) =>
        (
            let
                val tv0=t1erm_interp1(t0,env0)
            in
                case- tv0 of
                | VALtup(t1,t2) =>
                (
                    t2
                )
                | _ =>
                let 
                    val () = println!("t1msnd: Error reconciling tuple: ", t0, "; got: ", tv0)
                    val () = assertloc(false)
                in
                    exit(1)
                end
            end
        )
        | T1Mtup(t1, t2) => VALtup(t1erm_interp1(t1,env0),t1erm_interp1(t2,env0))
        | T1Manno(t0, _) => t1erm_interp1(t0,env0)
    )

    implement tdclist_interp1(t1dlc, env0) =
    (
        case t1dlc of
        | mylist_cons(hd, tl) =>
        (
            let
                val T1DCL(va0, t1m0) = hd
                val t1val = t1erm_interp1(t1m0, env0)
            in
                tdclist_interp1(tl, d1env_extend(env0, va0, t1val))
            end
        ) 
        | mylist_nil() => env0
    )

    implement tdclist_interp(t1dlc) = tdclist_interp1(t1dlc, d1env_nil())

    implement t1pgm_interp(t1pg) = 
    (
        let 
            val-T1PGM(t1dc, t1m0) = t1pg
        in
            t1erm_interp1(t1m0, tdclist_interp(t1dc))
        end
    )
end