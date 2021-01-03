# the compiler: gcc for C program, define as g++ for C++
CC = gcc

# compiler flags:
#  -g    adds debugging information to the executable file
#  -Wall turns on most, but not all, compiler warnings
CFLAGS  = -g -Wall

PROGS :=
PROGS += fibo
PROGS += fact

all:: $(PROGS)

%: %.lam.c; $(CC) -o $@ $< $(CFLAGS)

clean::
clean:: ; rm -f *.c
clean:: ; rm -f $(PROGS)