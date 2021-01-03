#include "share/atspre_staload.hats"

#staload "./project.sats"
#staload "./mylib.sats"
#staload "./mylib.dats"

implement fprint_val<t1erm> = fprint_t1erm
implement fprint_val<t1dcl> = fprint_t1dcl

(* ****** ****** *)

implement print_t1erm(t1m) = fprint_t1erm(stdout_ref, t1m)

implement fprint_t1erm(out, t1m) =
(
  case- t1m.node() of
  | T1Mnil() => fprint!(out, "T1Mnil(", ")")
  | T1Mbtf(x0) => fprint!(out, "T1Mbtf(", x0, ")")
  | T1Mint(x0) => fprint!(out, "T1Mint(", x0, ")")
  | T1Mstr(x0) => fprint!(out, "T1Mstr(", x0, ")")
  | T1Mvar(x0) => fprint!(out, "T1Mvar(", x0, ")")
  | T1Mlam(x0, t1p1, t1m2) => fprint!(out, "T1Mlam(", x0, "; ", t1p1, "; ", t1m2, ")")
  | T1Mapp(t1m1, t1m2) => fprint!(out, "T1Mapp(", t1m1, ";", t1m2, ")")
  | T1Mlet(t1dl, t1m1) => fprint!(out, "T1Mlet(", t1dl, "; ", t1m1, ")")
  | T1Mfix(t1v,t1p1, t1m1) => fprint!(out, "T1Mfix(", t1v, "; ", t1p1, "; ", t1m1, ")")
  | T1Mfst(tup) => fprint!(out, "T1Mfst(", tup, ")")
  | T1Msnd(tup) => fprint!(out, "T1Msnd(", tup, ")")
  | T1Mtup(t1m1, t1m2) => fprint!(out, "T1Mtup(", t1m1, ", ", t1m2, ")")
  | T1Mopr1(opr, t1m1) => fprint!(out, "T1Mopr1(", opr, ";", t1m1, ")")
  | T1Mopr2(opr, t1m1, t1m2) => fprint!(out, "T1Mopr2(", opr, ";", t1m1, ";", t1m2, ")")
  | T1Manno(t1m1, t1p2) => fprint!(out, "T1Manno(", t1m1, "; ", t1p2, ")")
  | T1Mcond(t1m1, t1m2, t1m3) => fprint!(out, "T1Mcond(", t1m1, "; ", t1m2, "; ", t1m3, ")")
)

(* ****** ****** *)

implement print_t1dcl(dcl) = fprint_t1dcl(stdout_ref, dcl)

implement fprint_t1dcl(out, dcl) = 
(
  case- dcl of
  | T1DCL(t1v, t1m) => fprint!(out, "T1DCL(", t1v, ", ", t1m, ")")
) (* end of [fprint_t1dcl] *)

(* ****** ****** *)

implement print_t1pgm(pgm) = fprint_t1pgm(stdout_ref, pgm)

implement fprint_t1pgm(out, pgm) =
(
  case+ pgm of
  | T1PGM(dcls, t1m1) => fprint!(out, "T1PGM(", dcls, "; ", t1m1, ")")
)

(* ****** ****** *)

local
  absimpl t1erm_tbox = $rec{t1erm_type= type1, t1erm_node= t1erm_node}
in(* in-of-local *)
  implement t1erm_get_node(t1m) = t1m.t1erm_node
  implement t1erm_get_type(t1m) = t1m.t1erm_type
  implement t1erm_make2(type, node) =
  (
    $rec{t1erm_type= type, t1erm_node= node}
  ) (* end of [t1erm_make2] *)
end // end of [local]

(* ****** ****** *)

implement t1erm_make1(node) = t1erm_make2(type, node) where
{
  val type = type1_new_ext()
} (* end of [t1erm_make1] *)

(* ****** ****** *)

implement t1erm_nil() = t1erm_make2(T1Pnil, T1Mnil())

implement t1erm_int(i0) = t1erm_make2(T1Pint, T1Mint(i0))

implement t1erm_btf(b0) = t1erm_make2(T1Pbool, T1Mbtf(b0))

implement t1erm_str(s0) = t1erm_make2(T1Pstring, T1Mstr(s0))

implement t1erm_tup(t1m0, t1m1) = 
(
  t1erm_make2(T1Ptup(t1m0.type(), t1m1.type()), T1Mtup(t1m0, t1m1))
)

implement t1erm_fst(t1m0) = 
(
  case- t1m0.type() of
  | T1Ptup(fst, snd) => 
  (
    t1erm_make2(fst, T1Mfst(t1m0))
  )
  | T1Pext(ext) =>
  (
    let
      val fst = type1_new_ext()
      val snd = type1_new_ext()
      val () = ext.set(T1Ptup(fst, snd))
    in
      t1erm_make2(fst, T1Mfst(t1m0))
    end
  )
)

implement t1erm_snd(t1m0) = 
(
  case- t1m0.type() of
  | T1Ptup(fst, snd) => 
  (
    t1erm_make2(snd, T1Msnd(t1m0))
  )
  | T1Pext(ext) =>
  (
    let
      val fst = type1_new_ext()
      val snd = type1_new_ext()
      val () = ext.set(T1Ptup(fst, snd))
    in
      t1erm_make2(snd, T1Msnd(t1m0))
    end
  )
)

implement t1erm_opr1(opr, t1m1) =
(
  if opr = "print" 
  then t1erm_make2(T1Pnil, T1Mopr1(opr, t1m1))
  else t1erm_make2(t1m1.type(), T1Mopr1(opr, t1m1))
)

implement t1erm_opr2(opr, t1m1, t1m2) =
(
  let
    val-true = type1_unify(t1m1.type(), t1m2.type())
  in
    case- opr of
    | ">" => t1erm_make2(T1Pbool, T1Mopr2(opr, t1m1, t1m2))
    | "<" => t1erm_make2(T1Pbool, T1Mopr2(opr, t1m1, t1m2))
    | ">=" => t1erm_make2(T1Pbool, T1Mopr2(opr, t1m1, t1m2))
    | "<=" => t1erm_make2(T1Pbool, T1Mopr2(opr, t1m1, t1m2))
    | "==" => t1erm_make2(T1Pbool, T1Mopr2(opr, t1m1, t1m2))
    | "!=" => t1erm_make2(T1Pbool, T1Mopr2(opr, t1m1, t1m2))
    | "=" => t1erm_make2(T1Pbool, T1Mopr2(opr, t1m1, t1m2))
    | _ => t1erm_make2(t1m1.type(), T1Mopr2(opr, t1m1, t1m2))
  end
)
(* ****** ****** *)

(* ****** ****** *)

(* end of [project_t1erm.dats] *)
