########################################################################
##

src_dir	 = ../src
inc_dir	 = ${src_dir}
comp_dir = ../compiler

include	../Make.common

PL_LIB	= ${src_dir}/${PLLIB}
PL_C	= ${comp_dir}/${PLC}
PLS	= $(wildcard *.pl)
TRASH	= core
CLEAN	= ${TRASH} $(PLS:%.pl=%.{c,h,o,mod,lnk.c,lnk.o}) $(PLS:%.pl=%)

########################################################################
.PHONY: FORCE

FORCE: ;

${PL_LIB}: FORCE
	@${MAKE} -C ${src_dir} $(notdir $@)
${PL_C}: FORCE
	@${MAKE} -C ${comp_dir} $(notdir $@)

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

%: %.pl ${PL_C} ${PL_LIB}
	../phase_1/plc $*.pl

%.mod: %.pl
	../phase_1/plc $*.pl
########################################################################

veryclean: FORCE
	do_all clean