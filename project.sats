(*
** For your
** final project
*)
(* ****** ****** *)
#staload "./mylib.sats"

(* ****** ****** *)
// LEVEL-0 SYNTAX
(* ****** ****** *)
typedef tpnam = string

(* ****** ****** *)
datatype type0 =
  | T0Pbas of tpnam
  | T0Pfun of (type0, type0)
  | T0Ptup of (type0, type0)   where type0lst  = mylist(type0)
and type0opt =
  | myoptn(type0)

(* ****** ****** *)
val T0Pnil: type0
val T0Pint: type0
val T0Pbool: type0
val T0Pstring: type0

(* ****** ****** *)
fun print_type0(xs : type0) : void

overload print with print_type0

fun fprint_type0(out : FILEref, xs : type0) : void

overload fprint with fprint_type0

(* ****** ****** *)
// creating an alias
typedef t0opr = string
typedef t0var = string

(* ****** ****** *)
// abstract syntax
datatype t0pgm =
  | T0PGM of (t0dclist, t0erm)
and t0dcl =
  | T0DCL of (t0var, t0erm)
and t0erm =
  | T0Mnil of ()
  | T0Mbtf of bool
  | T0Mint of (int)
  | T0Mflt of double
  | T0Mstr of string
  | T0Mvar of (t0var)
  | T0Mlam of (t0var, type0opt, t0erm, type0opt)
  | T0Mfix of (t0var, t0erm)
  | T0Mapp of (t0erm, t0erm)
  | T0Mlet of (t0dclist, t0erm)
  | T0Mopr1 of (t0opr, t0erm)
  | T0Mopr2 of (t0opr, t0erm, t0erm)
  | T0Moprs of (t0opr, t0ermlst)
  | T0Mtup of (t0ermlst)
  | T0Mprj of (t0erm, int)
  | T0Manno of (t0erm, type0)
  | T0Mcond of (t0erm, t0erm, t0ermopt)   where
                                          t0dclist  = mylist(t0dcl)
and t0ermlst =
  | mylist(t0erm)
and t0ermopt =
  | myoptn(t0erm)

(* ****** ****** *)
fun print_t0pgm(xs : t0pgm) : void

overload print with print_t0pgm

fun fprint_t0pgm(out : FILEref, xs : t0pgm) : void

overload fprint with fprint_t0pgm

(* ****** ****** *)
fun print_t0dcl(xs : t0dcl) : void

overload print with print_t0dcl

fun fprint_t0dcl(out : FILEref, xs : t0dcl) : void

overload fprint with fprint_t0dcl

(* ****** ****** *)
fun print_t0erm(xs : t0erm) : void

overload print with print_t0erm

fun fprint_t0erm(out : FILEref, xs : t0erm) : void

overload fprint with fprint_t0erm

(* ****** ****** *)
// LEVEL-1 SYNTAX
// for resolving bindings
(* ****** ****** *)
abstype tpext_type = ptr

typedef tpext = tpext_type

(* ****** ****** *)
fun print_tpext : tpext -> void

overload print with print_tpext

fun fprint_tpext : (FILEref, tpext) -> void

overload fprint with fprint_tpext

(* ****** ****** *)
datatype type1 =
  | T1Pbas of tpnam
  | T1Pfun of (type1, type1)
  | T1Ptup of (type1, type1)
  | T1Pext of tpext   where type1lst  = mylist(type1)
and type1opt =
  | myoptn(type1)

(* ****** ****** *)
fun tpext_new() : tpext

fun type1_new_ext() : type1

fun type1_new_tup() : type1

fun type1_new_fun() : type1

(* ****** ****** *)
fun print_type1(xs : type1) : void

overload print with print_type1

fun fprint_type1(out : FILEref, xs : type1) : void

overload fprint with fprint_type1

(* ****** ****** *)
typedef t1opt = myoptn(type1)

fun tpext_get : tpext -> t1opt

fun tpext_set(tpext, def : type1) : void

overload .get with tpext_get
overload .set with tpext_set

(* ****** ****** *)
// abstflt stamp_type = int
typedef stamp = stamp_type

// abstbox stamper_type = ptr
typedef stamper = stamper_type

(* ****** ****** *)
fun stamper_make() : stamper

fun stamper_stamp(stamper) : stamp

(* ****** ****** *)
fun print_stamp(stamp) : void

fun fprint_stamp(FILEref, stamp) : void

overload print with print_stamp
overload fprint with fprint_stamp

(* ****** ****** *)
abstype t1var_type = ptr

// 1 word
typedef t1var = t1var_type

// alias
(* ****** ****** *)
fun t1var_new(name : t0var) : t1var

(* ****** ****** *)
fun t1var_get_type(t1v0 : t1var) : type1

overload .type with t1var_get_type

(* ****** ****** *)
fun print_t1var(t1var) : void

fun fprint_t1var(out : FILEref, t1var) : void

overload print with print_t1var
overload fprint with fprint_t1var

(* ****** ****** *)
abstype t1erm_tbox = ptr

typedef t1erm = t1erm_tbox

(* ****** ****** *)
datatype t1pgm =
  | T1PGM of (t1dclist, t1erm)
and t1dcl =
  | T1DCL of (t1var, t1erm)
and t1erm_node =
  | T1Mnil of ()
  | T1Mbtf of bool
  | T1Mint of (int)
  | T1Mstr of string
  | T1Mvar of (t1var)
  | T1Mlam of (t1var, type1, t1erm)
  | T1Mfix of (t1var, type1, t1erm)
  | T1Mapp of (t1erm, t1erm)
  | T1Mlet of (t1dclist, t1erm)
  | T1Mopr1 of (t0opr, t1erm)
  | T1Mopr2 of (t0opr, t1erm, t1erm)
  | T1Mfst of (t1erm)
  | T1Msnd of (t1erm)
  | T1Mtup of (t1erm, t1erm)
  | T1Manno of (t1erm, type1)
  | T1Mcond of (t1erm, t1erm, t1erm)   where t1dclist  = mylist(t1dcl)
and t1ermlst =
  | mylist(t1erm)
and t1ermopt =
  | myoptn(t1erm)

(* ****** ****** *)
fun t1erm_get_node(t1erm) : t1erm_node

fun t1erm_get_type(t1m0 : t1erm) : type1

#symload .node with t1erm_get_node
#symload .type with t1erm_get_type
(* ****** ****** *)
fun print_t1erm(t1erm) : void

fun fprint_t1erm(FILEref, t1erm) : void

overload print with print_t1erm
overload fprint with fprint_t1erm

(* ****** ****** *)
fun print_t1dcl(t1dcl) : void

fun fprint_t1dcl(FILEref, t1dcl) : void

overload print with print_t1dcl
overload fprint with fprint_t1dcl

(* ****** ****** *)
fun print_t1pgm(t1pgm) : void

fun fprint_t1pgm(FILEref, t1pgm) : void

overload print with print_t1pgm
overload fprint with fprint_t1pgm

(* ****** ****** *)
fun trans01_tpgm : t0pgm -> t1pgm

(* ****** ****** *)
fun type1_unify(tp1 : type1, tp2 : type1) : bool

(* ****** ****** *)

fun t1pgm_tinfer(t1pgm) : void

fun t1dcl_tinfer(t1dcl) : void

fun t1erm_tinfer(t1erm) : void

(* ****** ****** *)
datatype value =
  | VALnil of ()
  | VALint of (int)
  | VALbtf of (bool)
  | VALstr of string
  | VALtup of (value, value)
  | VALlam of (t1erm, d1env)
  | VALfix of (t1var, value)
and d1env =
  | D1ENV of mylist(@(t1var, value))
where valuelst = mylist(value)
(* ****** ****** *)
fun print_value(value) : void

fun fprint_value(FILEref, value) : void

overload print with print_value
overload fprint with fprint_value

(* ****** ****** *)
fun t1pgm_interp(t1pgm) : value

(* ****** ****** *)
fun project_main0 { n : int | n >= 1 } (int(n), !argv(n)) : void

(* ****** ****** *)
(* ****** ****** *)
val T1Pnil: type1
val T1Pint: type1
val T1Pbool: type1
val T1Pstring: type1
val T1Pdouble: type1

(* ****** ****** *)
fun t1erm_make1(node : t1erm_node) : t1erm

fun t1erm_make2(type : type1, node : t1erm_node) : t1erm

(* ****** ****** *)
fun t1erm_nil() : t1erm

fun t1erm_int(i0 : int) : t1erm

fun t1erm_btf(b0 : bool) : t1erm

fun t1erm_str(s0 : string) : t1erm

fun t1erm_tup(t1m0 : t1erm, t1m1 : t1erm) : t1erm

fun t1erm_fst(t1m0 : t1erm) : t1erm

fun t1erm_snd(t1m0 : t1erm) : t1erm

fun t1erm_opr1(opr : string, t1m0 : t1erm) : t1erm

fun t1erm_opr2(opr : string, t1m0 : t1erm, t1m1 : t1erm) : t1erm

(* ****** ****** *)
datatype s0env =
  | S0ENV of mylist(@(t0var, t1erm))
(* ****** ****** *)
fun s0env_nil() : s0env

fun s0env_extend(env0 : s0env, t0v1 : t0var, t1m2 : t1erm) : s0env

fun s0env_search(env0 : s0env, t0v1 : t0var) : myoptn(t1erm)

fun d1env_nil() : d1env

fun d1env_extend(env0 : d1env, t0v1 : t1var, t1val : value) : d1env

fun d1env_search(env0 : d1env, t0v1 : t1var) : myoptn(value)

(* ****** ****** *)
fun t1var_get_name(t1v0 : t1var) : string

#symload .name with t1var_get_name

fun t1var_make(name : string, t1p0 : type1) : t1var

fun t1pgm_tinfer(t1pg : t1pgm) : type1

fun t1erm_tinfer(t1m0 : t1erm) : type1

fun t1dcl_tinfer(t1dcl : t1dclist) : void

typedef t2tmp = string

(* ****** ****** *)
fun t2tmp_new() : t2tmp

datatype t2val =
  | T2Vnil of ()
  | T2Vint of int
  | T2Vstr of string
  | T2Vbtf of bool
  | T2Vtmp of t2tmp
  | T2Vfun of t2fnm   where t2fnm  = string

fun print_t2val(t2val) : void

overload print with print_t2val

fun fprint_t2val(FILEref, t2val) : void

overload fprint with fprint_t2val

(* ****** ****** *)
datatype t2ins =
  | T2INSmov of (t2tmp, t2val)
  | T2INSclo of (t2tmp, t2fnm, t2valist, t2inslst)
  | T2INSpos of (t2tmp, t2val)
  | T2INSsub of (t2tmp, t2val)
  | T2INSneg of (t2tmp, t2val)
  | T2INSprint of (t2tmp, t2val)
  | T2INSiadd of (t2tmp, t2val, t2val)
  | T2INSisub of (t2tmp, t2val, t2val)
  | T2INSimul of (t2tmp, t2val, t2val)
  | T2INSidiv of (t2tmp, t2val, t2val)
  | T2INSimod of (t2tmp, t2val, t2val)
  | T2INSneq of (t2tmp, t2val, t2val)
  | T2INSeq of (t2tmp, t2val, t2val)
  | T2INSlt of (t2tmp, t2val, t2val)
  | T2INSlte of (t2tmp, t2val, t2val)
  | T2INSgt of (t2tmp, t2val, t2val)
  | T2INSgte of (t2tmp, t2val, t2val)
  | T2INStup of (t2tmp, t2val, t2val)
  | T2INSfst of (t2tmp, t2val)
  | T2INSsnd of (t2tmp, t2val)
  | T2INScall of (t2tmp, t2val, t2val)
  | T2INScond of (t2tmp, t2val, t2inslst, t2inslst)   where
                                                      t2inslst  = mylist(t2ins)
and t2valist =
  | mylist(t2val)

(* ****** ****** *)
fun print_t2ins(t2ins) : void

overload print with print_t2ins

fun fprint_t2ins(FILEref, t2ins) : void

overload fprint with fprint_t2ins

fun t2inslst_extend(t2inslst, t2ins) : t2inslst

fun t2inslst_append(t2inslst, t2inslst) : t2inslst

overload + with t2inslst_extend
overload + with t2inslst_append

datatype c2env =
  | C2ENVnil of ()
  | C2ENVmark of (int, c2env)
  | C2ENVcons of (t1var, t2val, c2env)

fun print_c2env(c2env) : void

overload print with print_c2env

fun fprint_c2env(FILEref, c2env) : void

overload fprint with fprint_c2env

datatype t2pgm =
  | T2PGM of (t2inslst, t2val, mylist(string), t2inslst)

fun print_t2pgm(t2pgm) : void

overload print with print_t2pgm

fun fprint_t2pgm(FILEref, t2pgm) : void

overload fprint with fprint_t2pgm

fun c2env_nil() : c2env

fun c2env_extend(c2env, t1var, t2val) : c2env

fun c2env_search(c2env, t1var) : myoptn(t2val)

fun trans12_term(env : c2env, t1m0 : t1erm) : (t2inslst, t2val)

fun trans12_tdcl(c2env, t1dcl) : (c2env, t2inslst)

fun trans12_tdclst(c2env, t1dclist) : (c2env, t2inslst)

fun trans12_tpgm(tpgm : t1pgm) : t2pgm

fun emitter(FILEref, t2pgm) : void

fun t2ins_emit(FILEref, t2ins) : void

(* end of [project.sats] *)
