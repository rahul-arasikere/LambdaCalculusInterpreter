#include "share/atspre_staload.hats"

#staload "./project.sats"
#staload "./mylib.sats"

local
  val stamper = stamper_make()
in
  fun t1var_stamp()=
    (stamper_stamp(stamper))
end

// end of [local]
(* ****** ****** *)
implement print_t1var (t1v) =
  fprint_t1var(stdout_ref, t1v)

(* ****** ****** *)
local
  absimpl t1var_type =
    $rec
    {
      t1var_name= t0var, 
      t1var_type= type1, 
      t1var_stamp= stamp
    }
in
  (* in-of-local *)
  implement t1var_new (name) =
    $rec
    {
      t1var_name= name, 
      t1var_type= type1,
      t1var_stamp= stamp
    } where
    {
      val stamp = t1var_stamp()
      val type1 = type1_new_ext()
    }
  implement t1var_get_type (t1v0) =
    t1v0.t1var_type
  
  implement t1var_get_name (t1v0) =
    t1v0.t1var_name
  
  implement fprint_t1var (out, t1v) =
    (fprint!(out, t1v.t1var_name))
  
  implement t1var_make (name, t1p0) =
    $rec
    {
      t1var_name = name,
      t1var_type = t1p0,
      t1var_stamp = stamp
    } where
    {
      val stamp = t1var_stamp()
    }
end

// end of [local]
(* ****** ****** *)
(* end of [project_t1var.dats] *)
