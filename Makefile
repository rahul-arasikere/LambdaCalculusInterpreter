######
#
# target : dependency ; command
#

PATSCC=patscc

XANADU = "./xanadu"
CFLAGS :=
CFLAGS += -DATS_MEMALLOC_GCBDW
INCLUDE :=
INCLUDE += -I$(XANADU)/srcgen/xats

OBJECTS :=
OBJECTS += project_stamp_dats.o
OBJECTS += project_type0_dats.o
OBJECTS += project_t0erm_dats.o
OBJECTS += project_tpext_dats.o
OBJECTS += project_type1_dats.o
OBJECTS += project_t1var_dats.o
OBJECTS += project_t1erm_dats.o
OBJECTS += project_s0env_dats.o
OBJECTS += project_d1env_dats.o
OBJECTS += project_c2env_dats.o
OBJECTS += project_trans01_dats.o
OBJECTS += project_trans12_dats.o
OBJECTS += project_tinfer_dats.o
OBJECTS += project_tinterp_dats.o
OBJECTS += project_t2ins_dats.o
OBJECTS += project_emitter_dats.o

lambda : main.dats $(OBJECTS); $(PATSCC) -o $@ $(INCLUDE) $(CFLAGS) $< $(OBJECTS) -L$(XANADU)/lib -lxatsopt -lgc

%_dats.o : %.dats ; $(PATSCC) -o $@ $(CFLAGS) -c $< 

libxatsopt: ; (cd $(XANADU); $(MAKE) -C srcgen/xats libxatsopt)

test::
test:: ; ./lambda TEST/fact.lam
test:: ; ./lambda TEST/fibo.lam
test:: ; ./lambda TEST/test03.lam
test:: ; ./lambda TEST/coin_change.lam
test:: ; ./lambda TEST/is_prime.lam
test:: ; $(MAKE) -C TEST all

clean:: ; rm -f *~
clean:: ; rm -f *_?ats.c
clean:: ; rm -f *_?ats.o
clean:: ; $(MAKE) -C TEST clean
clean:: ; rm -f lambda
clean:: ; (cd $(XANADU); $(MAKE) -C srcgen/xats cleanall)
