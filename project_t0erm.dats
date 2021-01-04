#include "share/atspre_staload.hats"

#staload "./project.sats"
#staload "./mylib.sats"
#staload "./mylib.dats"

implement fprint_val<t0erm> =
  fprint_t0erm

implement fprint_val<t0dcl> =
  fprint_t0dcl

implement print_t0erm (tm) =
  fprint_t0erm(stdout_ref, tm)

implement fprint_t0erm (out, tm0) =
  (case+ tm0 of
    | T0Mnil() => fprint!(out, "T0Mnil(", ")")
    | T0Mbtf (btf) => fprint!(out, "T0Mbtf(", btf, ")")
    | T0Mint (int) => fprint!(out, "T0Mint(", int, ")")
    | T0Mflt (flt) => fprint!(out, "T0Mflt(", flt, ")")
    | T0Mstr (str) => fprint!(out, "T0Mstr(", str, ")")
    | T0Mvar (t0v) => fprint!(out, "T0Mvar(", t0v, ")")
    | T0Mlam (t0v, tp0arg, tm1, tp0res) => fprint!( out
                                                  , "T0Mlam("
                                                  , t0v
                                                  , ", "
                                                  , tm1
                                                  , ", "
                                                  , ")"
                                                  )
    | T0Mfix (t0v, tm1) => fprint!(out, "T0Mfix(", t0v, ", ", tm1, ")")
    | T0Mapp (tm1, tm2) => fprint!(out, "T0Mapp(", tm1, ", ", tm2, ")")
    | T0Mlet (dc0lst, tm1) => fprint!( out
                                     , "T0Mlet("
                                     , dc0lst
                                     , ", "
                                     , tm1
                                     , ")"
                                     )
    | T0Mtup (tms) => fprint!(out, "T0Mtup(", tms, ")")
    | T0Mprj (tm1, idx) => fprint!(out, "T0Mprj(", tm1, ", ", idx, ")")
    | T0Mopr1 (opr, tm1) => fprint!(out, "T0Mopr1(", opr, ", ", tm1, ")")
    | T0Mopr2 (opr, tm1, tm2) => fprint!( out
                                        , "T0Mopr2("
                                        , opr
                                        , ", "
                                        , tm1
                                        , ", "
                                        , tm2
                                        , ")"
                                        )
    | T0Moprs (opr, tm0lst) => fprint!(out, "T0Moprs(", opr, ", ", tm0lst, ")")
    | T0Mcond (tm1, tm2, opt) => fprint!( out
                                        , "T0Mcond("
                                        , tm1
                                        , ", "
                                        , tm2
                                        , ", "
                                        , opt
                                        , ")"
                                        )
    | T0Manno (tm1, tp2) => fprint!(out, "T0Manno(", tm1, ", ", tp2, ")"))

implement print_t0dcl (tm) =
  fprint_t0dcl(stdout_ref, tm)

implement fprint_t0dcl (out, tdcl) =
  (case+ tdcl of
    | T0DCL (x0, def) => fprint!(out, "T0DCL(", x0, ", ", def, ")"))

implement print_t0pgm (pgm) =
  fprint_t0pgm(stdout_ref, pgm)

implement fprint_t0pgm (out, pgm) =
  (case+ pgm of
    | T0PGM (dcls, t0m1) => fprint!(out, "T0PGM(", dcls, "; ", t0m1, ")"))

(* end of [project_t0erm.dats] *)
