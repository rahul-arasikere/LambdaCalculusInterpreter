/* ****** ****** */
/*
A basic runtime
for the LAMBDA language
*/
/* ****** ****** */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdarg.h>

/* ****** ****** */

extern void *mymalloc(size_t);

/* ****** ****** */

void *mymalloc(size_t bsz)
{
  void *p0 = malloc(bsz);
  if (!p0)
  {
    fprintf(stderr, "mymalloc: the NULL pointer is returned\n");
    exit(1);
  }
  return p0;
}

/* ****** ****** */

#define TAGint 1
#define TAGstr 2
#define TAGclo 3 // closure function

/* ****** ****** */

typedef struct
{
  int tag;
} lamval_;

typedef lamval_ *lamval;

/* ****** ****** */

typedef struct
{
  int tag;
  int data;
} lamval_int_;
typedef struct
{
  int tag;
  char *data;
} lamval_str_;
typedef struct
{
  int tag;
  lamval *fenv;
} lamval_clo_;

typedef lamval_int_ *lamval_int;
typedef lamval_str_ *lamval_str;
typedef lamval_clo_ *lamval_clo;

/* ****** ****** */

int LAMVAL_tag(lamval x)
{
  return x->tag;
}

/* ****** ****** */

extern void LAMVAL_print(lamval);
extern void LAMVAL_fprint(FILE *out, lamval x);

/* ****** ****** */

void LAMVAL_print(lamval x)
{
  LAMVAL_fprint(stdout, x);
  return;
}

void LAMVAL_fprint(FILE *out, lamval x)
{

  switch (x->tag)
  {
  case TAGint:
    fprintf(out, "%i", ((lamval_int)x)->data);
    break;
  case TAGstr:
    fprintf(out, "%s", ((lamval_str)x)->data);
    break;
  case TAGclo:
    fprintf(out, "[LAMVAL_CLO]");

    break;
  default:
    fprintf(out, "[LAMVAL_???]");
    break;
  } // end of [switch]

  return;
}

/* ****** ****** */

lamval LAMVAL_int(int i)
{
  lamval_int p0;
  p0 = (lamval_int)mymalloc(sizeof(lamval_int_));
  p0->tag = TAGint;
  p0->data = i;
  return (lamval)p0;
}

lamval LAMVAL_btf(int i)
{
  lamval_int p0;
  p0 = (lamval_int)mymalloc(sizeof(lamval_int_));
  p0->tag = TAGint;
  p0->data = i;
  return (lamval)p0;
}

lamval LAMVAL_clo(lamval clo, ...)
{
  va_list arg;
  va_start(arg, clo);
  int argc = 0;
  while (va_arg(arg, lamval) != NULL)
    argc++;
  va_end(arg);
  lamval_clo p0;
  p0 = (lamval_clo)mymalloc(sizeof(lamval_clo_));
  p0->tag = TAGclo;
  p0->fenv = (lamval *)mymalloc(argc * sizeof(lamval));
  va_start(arg, clo);
  int i = 0;
  p0->fenv[i] = clo;
  for (i = 1; i < argc; i++)
    p0->fenv[i] = va_arg(arg, lamval);
  va_end(arg);
  return (lamval)p0;
}

lamval LAMVAL_get(lamval x, int i)
{
  return (lamval)(((lamval_clo)x)->fenv[i]);
}

/* ****** ****** */

lamval LAMOPR_ilt(lamval x, lamval y)
{
  /*
  assert(x->tag == TAGint);
  assert(y->tag == TAGint);
  */
  return LAMVAL_int((((lamval_int)x)->data < ((lamval_int)y)->data) ? 1 : 0);
}

lamval LAMOPR_igt(lamval x, lamval y)
{
  /*
  assert(x->tag == TAGint);
  assert(y->tag == TAGint);
  */
  return LAMVAL_int((((lamval_int)x)->data > ((lamval_int)y)->data) ? 1 : 0);
}

lamval LAMOPR_ieq(lamval x, lamval y)
{
  /*
  assert(x->tag == TAGint);
  assert(y->tag == TAGint);
  */
  return LAMVAL_int((((lamval_int)x)->data == ((lamval_int)y)->data) ? 1 : 0);
}

lamval LAMOPR_igte(lamval x, lamval y)
{
  /*
  assert(x->tag == TAGint);
  assert(y->tag == TAGint);
  */
  return LAMVAL_int((((lamval_int)x)->data >= ((lamval_int)y)->data) ? 1 : 0);
}

lamval LAMOPR_ilte(lamval x, lamval y)
{
  /*
  assert(x->tag == TAGint);
  assert(y->tag == TAGint);
  */
  return LAMVAL_int((((lamval_int)x)->data <= ((lamval_int)y)->data) ? 1 : 0);
}

lamval LAMOPR_ineq(lamval x, lamval y)
{
  /*
  assert(x->tag == TAGint);
  assert(y->tag == TAGint);
  */
  return LAMVAL_int((((lamval_int)x)->data != ((lamval_int)y)->data) ? 1 : 0);
}

/* ****** ****** */

lamval LAMOPR_iadd(lamval x, lamval y)
{
  /*
  assert(x->tag == TAGint);
  assert(y->tag == TAGint);
  */
  return LAMVAL_int(((lamval_int)x)->data + ((lamval_int)y)->data);
}

/* ****** ****** */

lamval LAMOPR_isub(lamval x, lamval y)
{
  /*
  assert(x->tag == TAGint);
  assert(y->tag == TAGint);
  */
  return LAMVAL_int(((lamval_int)x)->data - ((lamval_int)y)->data);
}

/* ****** ****** */

lamval LAMOPR_imul(lamval x, lamval y)
{
  /*
  assert(x->tag == TAGint);
  assert(y->tag == TAGint);
  */
  return LAMVAL_int(((lamval_int)x)->data * ((lamval_int)y)->data);
}

lamval LAMOPR_idiv(lamval x, lamval y)
{
  /*
  assert(x->tag == TAGint);
  assert(y->tag == TAGint);
  */
  return LAMVAL_int(((lamval_int)x)->data / ((lamval_int)y)->data);
}

lamval LAMOPR_imod(lamval x, lamval y)
{
  /*
  assert(x->tag == TAGint);
  assert(y->tag == TAGint);
  */
  return LAMVAL_int(((lamval_int)x)->data % ((lamval_int)y)->data);
}
/* ****** ****** */

lamval LAMVAL_call(lamval (*fn)(lamval arg), lamval clo, ...)
{
  return fn(LAMVAL_clo(clo));
}

/* end of [runtime.h] */
