########################################################################
##

src_dir	 = ../src
inc_dir	 = ${src_dir}
comp_dir = ../compiler

include	../Make.common

PL_LIB	= ${src_dir}/${PLLIB}
PL_C	= ${comp_dir}/${PLC}
PLC1	= ../phase_1/${PLC}
PLS	= $(wildcard *.pl)
TRASH	= core .plc1
CLEAN	= ${TRASH} $(PLS:%.pl=%.{c,h,o,mod,lnk.c,lnk.o}) $(PLS:%.pl=%)

########################################################################
.PHONY: FORCE

FORCE: ;

all: ${PLC1} ${PLC}


.plc1:
	@${MAKE} -C ../phase_1 ${PLC}
	touch .plc1

${COMP}: $*.mod

${PROG}: ${PROG}.pl ${PROG}.mod ${MODULES} ${PROG}.lnk.o ${PL_LIB}
	${CC} ${CFLAGS} $(filter %.o %.a,$^) -o $@
	-rm -f $@.mod # $@.lnk.c $@.c
# ${PROG}: ${PL_LIB} ${PROG}.pl ${PROG}.mod ;

# %: %.pl ${PL_C} ${PL_LIB}
# 	echo "'$*.pl'." | ../phase_1/plc
# 
# %.mod: %.pl
# 	echo "'$*.pl'." | ../phase_1/plc

%: %.pl .plc1
	${PLC1} $*.pl

%.mod: %.pl .plc1
	../phase_1/plc $*.pl
########################################################################

veryclean: FORCE
	do_all clean
