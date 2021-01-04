#include "share/atspre_staload.hats"

#staload "./project.sats"
#staload "./mylib.sats"

implement T1Pnil =
  T1Pbas("nil")

implement T1Pint =
  T1Pbas("int")

implement T1Pbool =
  T1Pbas("bool")

implement T1Pstring =
  T1Pbas("string")

implement type1_new_ext () =
  T1Pext(tpext_new())

local
  fun auxtype(t1p1 : type1, t1p2 : type1) : bool =
    (case- t1p1 of
      | T1Pbas (nm1) => (case+ t1p2 of
        | T1Pbas (nm2) => (nm1 = nm2)
        | _ => false)
      | T1Pfun (t1p11, t1p12) => (case+ t1p2 of
        | T1Pfun (t1p21, t1p22) => type1_unify(t1p11, t1p21)
        && type1_unify(t1p12, t1p22)
        | _ => false)
      | T1Ptup (t1p11, t1p12) => (case+ t1p2 of
        | T1Ptup (t1p21, t1p22) => type1_unify(t1p11, t1p21)
        && type1_unify(t1p12, t1p22)
        | _ => false))
in
  implement type1_unify (t1p1, t1p2) =
    (case+ t1p1 of
      | T1Pext (X1) => (case X1.get() of
        | myoptn_nil() => true where
        { val () = X1.set(t1p2) }
        | myoptn_cons (tp1) => type1_unify(tp1, t1p2))
      | _ => (case+ t1p2 of
        | T1Pext (X2) => (case X2.get() of
          | myoptn_nil() => true where
          { val () = X2.set(t1p1) }
          | myoptn_cons (tp2) => type1_unify(t1p1, tp2))
        | _ => auxtype(t1p1, t1p2)))
end

implement print_type1 (tm) =
  fprint_type1(stdout_ref, tm)

implement fprint_type1 (out, tm) =
  (case- tm of
    | T1Pbas (nam) => fprint!(out, "T1Pbas(", nam, ")")
    | T1Pfun (tp1, tp2) => fprint!(out, "T1Pfun(", tp1, ", ", tp2, ")")
    | T1Ptup (tp1, tp2) => fprint!(out, "T1Ptup(", tp1, ", ", tp2, ")")
    | T1Pext (X0) => fprint!(out, "T1Pext(", X0, ")"))

(* end of [project_type1.dats] *)
