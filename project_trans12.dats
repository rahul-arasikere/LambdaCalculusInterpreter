#include "share/atspre_staload.hats"
#include "share/atspre_staload_libats_ML.hats"

#staload "./project.sats"
#staload "./mylib.sats"

implement trans12_term (env, t1m0) =
  (let
    fun get_mark(env0 : c2env) : int =
      (case- env0 of
        | C2ENVmark (i0, _) => (i0)
        | C2ENVnil() => 0
        | C2ENVcons (_, _, env1) => get_mark(env1))
    
    fun trans12(tm : t1erm) : (t2inslst, t2val) =
      trans12_term(env, tm)
  in
    case- t1m0.node() of
      | T1Mnil() => (inss, T2Vnil()) where
      { val inss = mylist_nil() }
      | T1Mint (i0) => (inss, T2Vint(i0)) where
      { val inss = mylist_nil() }
      | T1Mbtf (b0) => (inss, T2Vbtf(b0)) where
      { val inss = mylist_nil() }
      | T1Mstr (s0) => (inss, T2Vstr(s0)) where
      { val inss = mylist_nil() }
      | T1Mvar (t1v) => (let
        val opt = c2env_search(env, t1v)
      in
        case- opt of
          | myoptn_cons (va0) => ((mylist_nil(), va0))
      end)
      | T1Mlam (t1v, t1p1, t1m1) => (let
        val new_env = c2env_extend(C2ENVmark( get_mark(env) + 1
                                            , env
                                            ), t1v, T2Vtmp(t1v.name()))
        val (ins1, val1) = trans12_term(new_env, t1m1)
      in
        (ins1, val1)
      end)
      | T1Mfix (t1v, t1p1, t1m1) => (let
        fun pull_lam_vals(t1m2) : t2valist =
          (case- t1m2.node() of
            | T1Mlam (t1v, _, t1) => mylist_cons( T2Vtmp(t1v.name())
                                                , pull_lam_vals(t1)
                                                )
            | _ => mylist_nil())
        
        val tmp = t2tmp_new()
        val fname = t1v.name()
        val new_env = C2ENVmark( get_mark(env) + 1
                               , c2env_extend(env, t1v, T2Vfun(fname))
                               )
        val (ins1, val1) = trans12_term(new_env, t1m1)
        val insmov = T2INSmov(tmp, val1)
        val inss = T2INSclo(tmp, fname, pull_lam_vals(t1m1), ins1 + insmov)
      in
        (mylist_cons(inss, mylist_nil()), T2Vfun(fname))
      end)
      | T1Mapp (t1m1, t1m2) => (let
        val tres = t2tmp_new()
        val (inss1, t1v1) = trans12(t1m1)
        val (inss2, t1v2) = trans12(t1m2)
        val ins3 = T2INScall(tres, t1v1, t1v2)
      in
        (inss1 + inss2 + ins3, T2Vtmp(tres))
      end)
      | T1Mlet (t1dclst, t1m1) => (let
        val tmp = t2tmp_new()
        val new_env = C2ENVmark(get_mark(env) + 1, env)
        val (new_env, inssdcl) = trans12_tdclst(new_env, t1dclst)
        val (inss1, val1) = trans12_term(new_env, t1m1)
      in end)
      | T1Mopr1 (opr, t1m1) => (let
        val tmp = t2tmp_new()
        val (ins1, val1) = trans12(t1m1)
      in
        case- opr of
          | "print" => (let
            val ins2 = T2INSprint(tmp, val1)
          in
            (ins1 + ins2, T2Vtmp(tmp))
          end)
          | "+" => (let
            val ins2 = T2INSpos(tmp, val1)
          in
            (ins1 + ins2, T2Vtmp(tmp))
          end)
          | "~" => (let
            val ins2 = T2INSneg(tmp, val1)
          in
            (ins1 + ins2, T2Vtmp(tmp))
          end)
          | "-" => (let
            val ins2 = T2INSsub(tmp, val1)
          in
            (ins1 + ins2, T2Vtmp(tmp))
          end)
      end)
      | T1Mopr2 (opr, t1m1, t1m2) => (let
        val tmp = t2tmp_new()
        val (ins1, val1) = trans12(t1m1)
        val (ins2, val2) = trans12(t1m2)
      in
        case- opr of
          | "+" => (let
            val ins3 = T2INSiadd(tmp, val1, val2)
          in
            (ins1 + ins2 + ins3, T2Vtmp(tmp))
          end)
          | "-" => (let
            val ins3 = T2INSisub(tmp, val1, val2)
          in
            (ins1 + ins2 + ins3, T2Vtmp(tmp))
          end)
          | "/" => (let
            val ins3 = T2INSidiv(tmp, val1, val2)
          in
            (ins1 + ins2 + ins3, T2Vtmp(tmp))
          end)
          | "%" => (let
            val ins3 = T2INSimod(tmp, val1, val2)
          in
            (ins1 + ins2 + ins3, T2Vtmp(tmp))
          end)
          | "*" => (let
            val ins3 = T2INSimul(tmp, val1, val2)
          in
            (ins1 + ins2 + ins3, T2Vtmp(tmp))
          end)
          | ">" => (let
            val ins3 = T2INSgt(tmp, val1, val2)
          in
            (ins1 + ins2 + ins3, T2Vtmp(tmp))
          end)
          | "<" => (let
            val ins3 = T2INSlt(tmp, val1, val2)
          in
            (ins1 + ins2 + ins3, T2Vtmp(tmp))
          end)
          | ">=" => (let
            val ins3 = T2INSgte(tmp, val1, val2)
          in
            (ins1 + ins2 + ins3, T2Vtmp(tmp))
          end)
          | "<=" => (let
            val ins3 = T2INSlte(tmp, val1, val2)
          in
            (ins1 + ins2 + ins3, T2Vtmp(tmp))
          end)
          | "!=" => (let
            val ins3 = T2INSneq(tmp, val1, val2)
          in
            (ins1 + ins2 + ins3, T2Vtmp(tmp))
          end)
          | "==" => (let
            val ins3 = T2INSeq(tmp, val1, val2)
          in
            (ins1 + ins2 + ins3, T2Vtmp(tmp))
          end)
          | "=" => (let
            val ins3 = T2INSeq(tmp, val1, val2)
          in
            (ins1 + ins2 + ins3, T2Vtmp(tmp))
          end)
      end)
      | T1Mtup (fst, snd) => (let
        val tmp = t2tmp_new()
        val (ins1, t2fst) = trans12(fst)
        val (ins2, t2snd) = trans12(snd)
        val ins3 = T2INStup(tmp, t2fst, t2snd)
      in
        (ins1 + ins2 + ins3, T2Vtmp(tmp))
      end)
      | T1Mfst (t1m1) => (let
        val tmp = t2tmp_new()
        val (ins1, t2m1) = trans12(t1m1)
        val ins2 = T2INSfst(tmp, t2m1)
      in
        (ins1 + ins2, T2Vtmp(tmp))
      end)
      | T1Msnd (t1m1) => (let
        val tmp = t2tmp_new()
        val (ins1, t2m1) = trans12(t1m1)
        val ins2 = T2INSsnd(tmp, t2m1)
      in
        (ins1 + ins2, T2Vtmp(tmp))
      end)
      | T1Manno (t1m1, t1p1) => (let
        val tmp = t2tmp_new()
        val (ins1, val1) = trans12(t1m1)
        val inss = mylist_nil()
      in
        (ins1 + inss, val1)
      end)
      | T1Mcond (t1cond, t1m1, t1m2) => (let
        val tmp = t2tmp_new()
        val (inscond, condval) = trans12(t1cond)
        val (insthen, thenval) = trans12(t1m1)
        val insthenmove = T2INSmov(tmp, thenval)
        val (inselse, elseval) = trans12(t1m2)
        val inselsemove = T2INSmov(tmp, elseval)
        val ins3 = T2INScond( tmp
                            , condval
                            , insthen + insthenmove
                            , inselse + inselsemove
                            )
      in
        (inscond + ins3, T2Vtmp(tmp))
      end)
  end)

implement trans12_tdcl (env, dcl) =
  (let
    val- T1DCL (tv0, t1m0) = dcl
  in
    case- t1m0.node() of
      | T1Mlam _ => (let
        val (tins, t2v0) = trans12_term( env
                                       , t1erm_make2(tv0.type(), T1Mfix(tv0, tv0.type(), t1m0))
                                       )
      in
        (c2env_extend(env, tv0, T2Vfun(tv0.name())), tins)
      end)
      | _ => (let
        val (tins, t2v0) = trans12_term(env, t1m0)
        val insmov = T2INSmov(tv0.name(), t2v0)
      in
        (c2env_extend(env, tv0, T2Vtmp(tv0.name())), tins + insmov)
      end)
  end)

implement trans12_tdclst (env, dclst) =
  (case- dclst of
    | mylist_cons (hd, tl) => (let
      val (new_env, ins1) = trans12_tdcl(env, hd)
      val (env1, ins2) = trans12_tdclst(new_env, tl)
    in
      (env1, ins1 + ins2)
    end)
    | mylist_nil() => (env, mylist_nil()))

local
  fun convert_dcl_to_string(env : c2env) : mylist(string) =
    (case- env of
      | C2ENVnil() => mylist_nil()
      | C2ENVmark (_, tl) => convert_dcl_to_string(tl)
      | C2ENVcons (t1v, _, tl) => mylist_cons( t1v.name()
                                             , convert_dcl_to_string(tl)
                                             ))
in
  implement trans12_tpgm (pgm0) =
    (let
      val- T1PGM (dclst, t1m0) = pgm0
      val (env, insdcl) = trans12_tdclst(c2env_nil(), dclst)
      val (ins, val1) = trans12_term(env, t1m0)
    in
      T2PGM(ins, val1, convert_dcl_to_string(env), insdcl)
    end)
end

(* end of [project_trans12.dats] *)
