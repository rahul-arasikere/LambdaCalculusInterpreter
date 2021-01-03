#include "share/atspre_staload.hats"

#staload "./project.sats"
#staload "./mylib.sats"

implement c2env_nil() = C2ENVmark(0, C2ENVnil())

implement c2env_extend(env, t1v, va0)= 
(
  C2ENVcons(t1v, va0, env)
)

implement c2env_search(env, t1v) = 
(
  case- env of
  | C2ENVnil() => myoptn_nil()
  | C2ENVcons(t1v0, va0, env1) =>
  (
    if (t1v0.name() = t1v.name())
    then myoptn_cons(va0)
    else c2env_search(env1, t1v)
  )
  | C2ENVmark(i0, env1) => c2env_search(env1, t1v)
)

implement print_c2env(env) = fprint_c2env(stdout_ref, env)

implement fprint_c2env(out, env) =
(
  case- env of
  | C2ENVnil() => fprint!(out, "C2ENVnil(", ")")
  | C2ENVmark(i0, env1) => fprint!(out, "C2ENVmark(", i0, "; ", env1, ")")
  | C2ENVcons(t1v, t2v, env1)  => fprint!(out, "C2ENVcons(", t1v, "; ", t2v, "; ", env1, ")")
)