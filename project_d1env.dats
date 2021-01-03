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

implement d1env_nil() = D1ENV(mylist_nil())

implement d1env_extend(env, x0, tm) =
let
  val+ D1ENV(xts) = env
in
  D1ENV(mylist_cons((x0, tm), xts))
end // end of [s0env_extend]

(* ****** ****** *)
implement d1env_search(env, x0) =
(
  auxlst(xts)
) where
{
  val+ D1ENV(xts) = env
  fun auxlst( xts: mylist@(t1var, value)) : myoptn(value) =
  (
    case+ xts of
    | mylist_nil() => myoptn_nil()
    | mylist_cons(xt1, xts) =>
    (
      if (x0.name() = xt1.0.name())
      then myoptn_cons(xt1.1) 
      else auxlst(xts)
    )
  )
} (*where*) // end of [s0env_search]