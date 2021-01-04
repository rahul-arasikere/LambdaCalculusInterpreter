#include "share/atspre_staload.hats"
#include "share/atspre_staload_libats_ML.hats"

#staload "./project.sats"
#staload "./mylib.sats"
#staload "./mylib.dats"

implement fprint_val<t2val> =
  fprint_t2val

implement fprint_val<t2ins> =
  fprint_t2ins

local
  #define BUFSZ 128
  
  val count = ref<int>(0)
  
  fun genNewName(prfx : string) : string =
    let
      val n = !count
      val () = !count := n + 1
    in
      prfx + strptr2string(g0int2string(n))
    end
in
  implement t2tmp_new () =
    (genNewName("tmp"))
end

// end[local]
implement t2inslst_append (t2m0lst, t2m1lst) =
  (case t2m1lst of
    | mylist_cons (hd, tl) => (let
      val t2m0lst_new = t2inslst_extend(t2m0lst, hd)
    in
      t2inslst_append(t2m0lst_new, tl)
    end)
    | mylist_nil() => t2m0lst)

implement t2inslst_extend (t2m0lst, t2m0) =
  (case t2m0lst of
    | mylist_cons (hd, tl) => mylist_cons(hd, t2inslst_extend(tl, t2m0))
    | mylist_nil() => mylist_cons(t2m0, mylist_nil()))

implement print_t2pgm (pgm0) =
  fprint_t2pgm(stdout_ref, pgm0)

implement fprint_t2pgm (out, pgm0) =
  (case- pgm0 of
    | T2PGM (inslst, val1, dcl, inslst_dcl) => fprint!( out
                                                      , "T2PGM("
                                                      , inslst
                                                      , "; "
                                                      , val1
                                                      , "; "
                                                      , dcl
                                                      , "; "
                                                      , inslst_dcl
                                                      , ")"
                                                      ))

implement print_t2val (t2v) =
  fprint_t2val(stdout_ref, t2v)

implement fprint_t2val (out, t2v) =
  (case- t2v of
    | T2Vnil() => fprint!(out, "T2Vnil()")
    | T2Vint (i0) => fprint!(out, "T2Vint(", i0, ")")
    | T2Vstr (s0) => fprint!(out, "T2Vstr(", s0, ")")
    | T2Vbtf (b0) => fprint!(out, "T2Vbtf(", b0, ")")
    | T2Vtmp (tmp) => fprint!(out, "T2Vtmp(", tmp, ")")
    | T2Vfun (fname) => fprint!(out, "T2Vfun(", fname, ")"))

implement print_t2ins (t2m0) =
  fprint_t2ins(stdout_ref, t2m0)

implement fprint_t2ins (out, t2m0) =
  (case- t2m0 of
    | T2INSmov (tmp, t2v1) => fprint!( out
                                     , "T2INSmov("
                                     , tmp
                                     , "; "
                                     , t2v1
                                     , ")"
                                     )
    | T2INSclo (tmp, fname, t2valst, t2lst) => fprint!( out
                                                      , "T2INSclo("
                                                      , tmp
                                                      , "; "
                                                      , fname
                                                      , "; "
                                                      , t2valst
                                                      , "; "
                                                      , t2lst
                                                      , ")"
                                                      )
    | T2INSpos (tmp, t2v1) => fprint!( out
                                     , "T2INSpos("
                                     , tmp
                                     , "; "
                                     , t2v1
                                     , ")"
                                     )
    | T2INSsub (tmp, t2v1) => fprint!( out
                                     , "T2INSsub("
                                     , tmp
                                     , "; "
                                     , t2v1
                                     , ")"
                                     )
    | T2INSneg (tmp, t2v1) => fprint!( out
                                     , "T2INSneg("
                                     , tmp
                                     , "; "
                                     , t2v1
                                     , ")"
                                     )
    | T2INSprint (tmp, t2v1) => fprint!( out
                                       , "T2INSprint("
                                       , tmp
                                       , "; "
                                       , t2v1
                                       , ")"
                                       )
    | T2INSiadd (tmp, t2v1, t2v2) => fprint!( out
                                            , "T2INSiadd("
                                            , tmp
                                            , "; "
                                            , t2v1
                                            , "; "
                                            , t2v2
                                            , ")"
                                            )
    | T2INSisub (tmp, t2v1, t2v2) => fprint!( out
                                            , "T2INSisub("
                                            , tmp
                                            , "; "
                                            , t2v1
                                            , "; "
                                            , t2v2
                                            , ")"
                                            )
    | T2INSimul (tmp, t2v1, t2v2) => fprint!( out
                                            , "T2INSimul("
                                            , tmp
                                            , "; "
                                            , t2v1
                                            , "; "
                                            , t2v2
                                            , ")"
                                            )
    | T2INSidiv (tmp, t2v1, t2v2) => fprint!( out
                                            , "T2INSidiv("
                                            , tmp
                                            , "; "
                                            , t2v1
                                            , "; "
                                            , t2v2
                                            , ")"
                                            )
    | T2INSimod (tmp, t2v1, t2v2) => fprint!( out
                                            , "T2INSimod("
                                            , tmp
                                            , "; "
                                            , t2v1
                                            , "; "
                                            , t2v2
                                            , ")"
                                            )
    | T2INSneq (tmp, t2v1, t2v2) => fprint!( out
                                           , "T2INSneq("
                                           , tmp
                                           , "; "
                                           , t2v1
                                           , "; "
                                           , t2v2
                                           , ")"
                                           )
    | T2INSeq (tmp, t2v1, t2v2) => fprint!( out
                                          , "T2INSeq("
                                          , tmp
                                          , "; "
                                          , t2v1
                                          , "; "
                                          , t2v2
                                          , ")"
                                          )
    | T2INSlt (tmp, t2v1, t2v2) => fprint!( out
                                          , "T2INSlt("
                                          , tmp
                                          , "; "
                                          , t2v1
                                          , "; "
                                          , t2v2
                                          , ")"
                                          )
    | T2INSlte (tmp, t2v1, t2v2) => fprint!( out
                                           , "T2INSlte("
                                           , tmp
                                           , "; "
                                           , t2v1
                                           , "; "
                                           , t2v2
                                           , ")"
                                           )
    | T2INSgt (tmp, t2v1, t2v2) => fprint!( out
                                          , "T2INSgt("
                                          , tmp
                                          , "; "
                                          , t2v1
                                          , "; "
                                          , t2v2
                                          , ")"
                                          )
    | T2INSgte (tmp, t2v1, t2v2) => fprint!( out
                                           , "T2INSgte("
                                           , tmp
                                           , "; "
                                           , t2v1
                                           , "; "
                                           , t2v2
                                           , ")"
                                           )
    | T2INStup (tmp, t2v1, t2v2) => fprint!( out
                                           , "T2INStup("
                                           , tmp
                                           , "; "
                                           , t2v1
                                           , "; "
                                           , t2v2
                                           , ")"
                                           )
    | T2INSfst (tmp, t2v1) => fprint!( out
                                     , "T2INSfst("
                                     , tmp
                                     , "; "
                                     , t2v1
                                     , ")"
                                     )
    | T2INSsnd (tmp, t2v1) => fprint!( out
                                     , "T2INSsnd("
                                     , tmp
                                     , "; "
                                     , t2v1
                                     , ")"
                                     )
    | T2INScall (tmp, t2v1, t2v2) => fprint!( out
                                            , "T2INScall("
                                            , tmp
                                            , "; "
                                            , t2v1
                                            , "; "
                                            , t2v2
                                            , ")"
                                            )
    | T2INScond (tmp, t2v1, t2m1lst, t2m2lst) => fprint!( out
                                                        , "T2INScond("
                                                        , tmp
                                                        , "; "
                                                        , t2v1
                                                        , "; "
                                                        , t2m1lst
                                                        , "; "
                                                        , t2m2lst
                                                        , ")"
                                                        ))
