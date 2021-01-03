#include "share/atspre_staload.hats"

#staload "./project.sats"
#staload "./mylib.sats"


implement print_type0(tm) = fprint_type0(stdout_ref, tm)

(* ****** ****** *)

implement fprint_type0(out, tm) =
(
  case- tm of
  | T0Pbas(nam) => fprint!(out, "T0Pbas(", nam, ")")
  | T0Pfun(tp1, tp2) => fprint!(out, "T0Pfun(", tp1, ", ", tp2, ")")
  | T0Ptup(tp1, tp2) => fprint!(out, "T0Ptup(", tp1, ", ", tp2, ")")
)

(* ****** ****** *)

(* end of [project_type0.dats] *)
