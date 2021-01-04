#include "share/atspre_staload.hats"

#staload "./project.sats"
#staload "./mylib.sats"

extern
fun trans01_type : type0 -> type1

extern
fun trans01_term(s0env, t0erm) : t1erm

extern
fun trans01_tdcl(s0env, t0dcl) : t1dcl

implement trans01_tdcl (env, td0) =
  let
    val T0DCL (va, tm0) = td0
  in
    T1DCL(t1var_new(va), trans01_term(env, tm0))
  end

local
  fun auxoprs(opr : string, t0mlst : mylist(t0erm), env0 : s0env) :
    t1erm =
    (let
      fun aux(t0m0 : mylist(t0erm)) : t1erm =
        (case t0m0 of
          | mylist_cons (hd, tl) => (case tl of
            | mylist_cons (hd1, tl1) => t1erm_opr2( opr
                                                  , trans01_term(env0, hd)
                                                  , aux(tl)
                                                  )
            | mylist_nil() => trans01_term(env0, hd))
          | mylist_nil() => t1erm_nil())
    in
      aux(t0mlst)
    end)
  
  fun auxtup(tp0 : mylist(t0erm), env0 : s0env) : t1erm =
    (case tp0 of
      | mylist_cons (hd, tl) => (case tl of
        | mylist_cons (hd1, tl1) => t1erm_tup(trans01_term( env0
                                                          , hd
                                                          ), auxtup(tl, env0))
        | mylist_nil() => (case hd of
          | T0Mtup _ => trans01_term(env0, hd)
          | _ => t1erm_tup(trans01_term(env0, hd), t1erm_nil())))
      | mylist_nil() => t1erm_tup(t1erm_nil(), t1erm_nil()))
  
  fun type0_to_type1(tp0 : type0) : type1 =
    (case+ tp0 of
      | T0Pbas (b) => T1Pbas(b)
      | T0Pfun (t0, t1) => T1Pfun(type0_to_type1(t0), type0_to_type1(t1))
      | T0Ptup (fst, snd) => T1Ptup( type0_to_type1(fst)
                                   , T1Ptup(type0_to_type1(fst), T1Pnil)
                                   ))
  
  fun trans01_tdcllist(env : s0env, tdclist : t0dclist) : t1dclist =
    (case tdclist of
      | mylist_cons (hd, tl) => (mylist_cons(trans01_tdcl( env
                                                         , hd
                                                         ), trans01_tdcllist(env, tl)))
      | mylist_nil() => mylist_nil())
  
  fun tdcl_to_senv(env : s0env, tdcl : t1dclist) : s0env =
    (case tdcl of
      | mylist_cons (hd, tl) => (let
        val- T1DCL (v0, tm0) = hd
        val va0 = v0.name()
        val tp0 = v0.type()
      in
        tdcl_to_senv(s0env_extend(env, va0, tm0), tl)
      end)
      | mylist_nil() => env)
in
  implement trans01_term (env0, tm0) =
    (let
      fun trans01(tm : t0erm) : t1erm =
        trans01_term(env0, tm)
    in
      case- tm0 of
        | T0Mnil _ => t1erm_nil()
        | T0Mint (i0) => t1erm_int(i0)
        | T0Mbtf (b0) => t1erm_btf(b0)
        | T0Mstr (s0) => t1erm_str(s0)
        | T0Mflt _ => exit(1)
        | T0Mvar (t0v) => (let
          val o0pt = s0env_search(env0, t0v)
        in
          case- o0pt of
            | myoptn_cons (t1m) => (t1m)
            | myoptn_nil() => (let
              val t1v = t1var_new(t0v)
            in
              t1erm_make2(t1v.type(), T1Mvar(t1v))
            end)
        end)
        | T0Mlam (t0v, tp0arg, t0m1, tp0res) => (let
          val tp1arg = (case tp0arg of
            | myoptn_cons (tp0) => type0_to_type1(tp0)
            | myoptn_nil() => type1_new_ext()) : type1
          val tp1res = (case tp0res of
            | myoptn_cons (tp0) => type0_to_type1(tp0)
            | myoptn_nil() => type1_new_ext()) : type1
          val t1v = t1var_make(t0v, tp1arg)
          val new_env = s0env_extend( env0
                                    , t0v
                                    , t1erm_make2(t1v.type(), T1Mvar(t1v))
                                    )
          val t1m1 = trans01_term(new_env, t0m1)
        in
          t1erm_make2(T1Pfun(tp1arg, tp1res), T1Mlam(t1v, tp1res, t1m1))
        end)
        | T0Mfix (t0v, t0m1) => (let
          val t1p0 = type1_new_ext()
          val t1v = t1var_make(t0v, t1p0)
          val t1m1 = trans01(t0m1)
        in
          t1erm_make2(t1v.type(), T1Mfix(t1v, t1v.type(), t1m1))
        end)
        | T0Mapp (t0m1, t0m2) => (let
          val t1m1 = trans01(t0m1)
          val t1m2 = trans01(t0m2)
        in
          case- t1m1.type() of
            | T1Pfun (targ, tres) => (t1erm_make2(tres, T1Mapp(t1m1, t1m2)))
            | T1Pext (ext) => (let
              val targ = type1_new_ext()
              val tres = type1_new_ext()
              val- true = type1_unify(targ, t1m2.type())
              val () = ext.set(T1Pfun(targ, tres))
            in
              t1erm_make2(tres, T1Mapp(t1m1, t1m2))
            end)
        end)
        | T0Mlet (dclist0, t0m1) => (let
          val dclist1 = trans01_tdcllist(env0, dclist0)
          val new_env = tdcl_to_senv(env0, dclist1)
          val t1m1 = trans01_term(new_env, t0m1)
        in
          t1erm_make2(t1m1.type(), T1Mlet(dclist1, t1m1))
        end)
        | T0Mopr1 (opr, t0m1) => (let
          val t1m1 = trans01(t0m1)
        in
          t1erm_opr1(opr, t1m1)
        end)
        | T0Mopr2 (opr, t0m1, t0m2) => (let
          val t1m1 = trans01(t0m1)
          val t1m2 = trans01(t0m2)
        in
          t1erm_opr2(opr, t1m1, t1m2)
        end)
        | T0Moprs (opr, t0m1lst) => (case t0m1lst of
          | mylist_cons (hd, tl) => (case tl of
            | mylist_cons (hd1, tl1) => auxoprs(opr, t0m1lst, env0)
            | mylist_nil() => trans01(T0Mopr1(opr, hd)))
          | mylist_nil() => t1erm_nil())
        | T0Mtup (t0m1lst) => (case t0m1lst of
          | mylist_cons (hd, tl) => (case tl of
            | mylist_cons (hd1, tl1) => t1erm_tup(trans01(hd), auxtup(tl, env0))
            | mylist_nil() => trans01(hd))
          | mylist_nil() => t1erm_nil())
        | T0Mprj (t0m1, i0) => (let
          fun get_ith_element(t0 : t1erm, i : int) : t1erm =
            (let
              
            in
              if i > 0 then
                get_ith_element(t1erm_snd(t0), i - 1)
              else
                t1erm_fst(t0)
            end)
          
          fun get_actual_element(t0mtemp : t0erm, i : int) : (int, t0erm) =
            (case- t0mtemp of
              | T0Mtup (t0mtlst) => (case- t0mtlst of
                | mylist_cons (T0Mprj (t0m2, i1), _) => get_actual_element( t0m2
                                                                          , i + i1
                                                                          )
                | _ => exit(0))
              | _ => (i, t0mtemp))
          
          val (index, temp) = get_actual_element(t0m1, i0)
          val t1m1 = trans01(temp)
        in
          get_ith_element(t1m1, index)
        end)
        | T0Manno (t0m1, t0p1) => (let
          val t1m1 = trans01(t0m1)
          val t1p0 = type0_to_type1(t0p1)
          val- true = type1_unify(t1m1.type(), t1p0)
        in
          t1erm_make2(t1p0, T1Manno(t1m1, t1m1.type()))
        end)
        | T0Mcond (t0m1, t0m2, o0pt) => (let
          val t1p2 = trans01(t0m2)
        in
          case o0pt of
            | myoptn_cons (t0m3) => (let
              val t1p3 = trans01(t0m3)
              val- true = type1_unify(t1p2.type(), t1p3.type())
            in
              t1erm_make2(t1p2.type(), T1Mcond(trans01(t0m1), t1p2, t1p3))
            end)
            | myoptn_nil() => t1erm_make2( t1p2.type()
                                         , T1Mcond(trans01(t0m1), t1p2, t1erm_nil())
                                         )
        end)
    end)
  
  implement trans01_tpgm (tm : t0pgm) =
    let
      val- T0PGM (dclist0, t0m1) = tm
      val dclist1 = trans01_tdcllist(s0env_nil(), dclist0)
      val new_env = tdcl_to_senv(s0env_nil(), dclist1)
    in
      T1PGM(dclist1, trans01_term(new_env, t0m1))
    end
end

(* end of [project_trans01.dats] *)
