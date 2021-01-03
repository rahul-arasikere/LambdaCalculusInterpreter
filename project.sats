#include "./../project.sats"

(* ****** ****** *)

val T1Pnil: type1
val T1Pint: type1
val T1Pbool: type1
val T1Pstring: type1
val T1Pdouble: type1

(* ****** ****** *)
//
fun t1erm_make1( node: t1erm_node): t1erm
fun t1erm_make2( type: type1, node: t1erm_node): t1erm
//
(* ****** ****** *)
fun t1erm_nil(): t1erm
fun t1erm_int(i0: int): t1erm
fun t1erm_btf(b0: bool): t1erm
fun t1erm_str(s0: string): t1erm
fun t1erm_tup(t1m0: t1erm, t1m1: t1erm): t1erm
fun t1erm_fst(t1m0: t1erm): t1erm
fun t1erm_snd(t1m0: t1erm): t1erm
fun t1erm_opr1(opr: string, t1m0: t1erm): t1erm
fun t1erm_opr2(opr: string, t1m0: t1erm, t1m1: t1erm): t1erm
(* ****** ****** *)
//
datatype s0env =
| S0ENV of mylist(@(t0var, t1erm))
//
(* ****** ****** *)
//
fun s0env_nil(): s0env
fun s0env_extend( env0: s0env, t0v1: t0var, t1m2: t1erm): s0env
//
fun s0env_search( env0: s0env, t0v1: t0var): myoptn(t1erm)

fun d1env_nil(): d1env

fun d1env_extend( env0: d1env, t0v1: t1var, t1val: value): d1env
//
fun d1env_search( env0: d1env, t0v1: t1var): myoptn(value)
//
(* ****** ****** *)
fun t1var_get_name (t1v0: t1var): string
#symload .name with t1var_get_name
fun t1var_make(name:string, t1p0: type1): t1var


fun t1pgm_tinfer(t1pg: t1pgm): type1
fun t1erm_tinfer(t1m0: t1erm): type1
fun t1dcl_tinfer(t1dcl: t1dclist): void


typedef t2tmp = string
(* ****** ****** *)

fun t2tmp_new(): t2tmp

datatype t2val =
| T2Vnil of ()
| T2Vint of int
| T2Vstr of string
| T2Vbtf of bool
| T2Vtmp of t2tmp
| T2Vfun of t2fnm
where t2fnm = string
//
fun print_t2val(t2val): void
overload print with print_t2val

fun fprint_t2val(FILEref, t2val): void
overload fprint with fprint_t2val

(* ****** ****** *)
datatype t2ins = 
| T2INSmov of (t2tmp, t2val) // t2tmp <- t2val
| T2INSclo of (t2tmp, t2fnm, t2valist, t2inslst) //changes
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
| T2INScall of (t2tmp, t2val(*fun*), t2val(*arg*))
| T2INScond of (t2tmp, t2val(*cond*), t2inslst(*then*), t2inslst(*else*)) //move result into t2tmp
where t2inslst = mylist(t2ins) and t2valist = mylist(t2val)
//
(* ****** ****** *)
//
fun print_t2ins(t2ins): void
overload print with print_t2ins

fun fprint_t2ins(FILEref, t2ins): void
overload fprint with fprint_t2ins



fun t2inslst_extend(t2inslst, t2ins): t2inslst
fun t2inslst_append(t2inslst, t2inslst): t2inslst
overload + with t2inslst_extend
overload + with t2inslst_append
(* ****** ****** *)

// abstype t2dcl_type = ptr
// typedef t2dcl = t2dcl_type

(* ****** ****** *)

datatype c2env =
| C2ENVnil of ()
| C2ENVmark of (int, c2env)
| C2ENVcons of (t1var, t2val, c2env)

fun print_c2env(c2env): void
overload print with print_c2env

fun fprint_c2env(FILEref, c2env): void
overload fprint with fprint_c2env

datatype t2pgm =
| T2PGM of (t2inslst, t2val, mylist(string), t2inslst)

fun print_t2pgm(t2pgm): void
overload print with print_t2pgm

fun fprint_t2pgm(FILEref, t2pgm): void
overload fprint with fprint_t2pgm

fun c2env_nil(): c2env

fun c2env_extend(c2env, t1var, t2val): c2env

fun c2env_search(c2env, t1var): myoptn(t2val)

fun trans12_term(env: c2env, t1m0: t1erm): (t2inslst, t2val)

fun trans12_tdcl(c2env, t1dcl): (c2env, t2inslst)

fun trans12_tdclst(c2env, t1dclist): (c2env, t2inslst)

fun trans12_tpgm(tpgm: t1pgm): t2pgm

fun emitter(FILEref, t2pgm): void

fun t2ins_emit(FILEref, t2ins): void

(* end of [project.sats] *)

