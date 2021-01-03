(* ****** ****** *)
#staload "./project.sats"
(* ****** ****** *)
#include
"share/atspre_staload.hats"
(* ****** ****** *)
#staload
"./../../mylib/mylib.sats"
#staload
"./../../mylib/mylib.dats"
(* ****** ****** *)

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
