########################################################################
##

src_dir	 = ../src
inc_dir	 = ${src_dir}
comp_dir = ../compiler

include	../Make.common

SRC = \
	comp.pl \
	plc.pl \


PL_LIB	= ${src_dir}/${PLLIB}
PL_C	= ${comp_dir}/${PLC}
PLC1	= ${comp_dir}/${PLC}
PLS	= $(wildcard *.pl)
TRASH	= 
CLEAN	= ${TRASH} $(PLS:%.pl=%.{c,h,o,mod,lnk.c,lnk.o}) $(PLS:%.pl=%)

########################################################################
.PHONY: FORCE

FORCE: ;

all: ${PLC}

${PL_LIB}:
	@${MAKE} -C ${src_dir} $(notdir $@)
${PLC1}:
	@${MAKE} -C ${comp_dir} $(notdir $@)

${COMP}: $*.mod

${PROG}: ${PROG}.pl ${PROG}.mod ${MODULES} ${PROG}.lnk.o ${PL_LIB}
	${CC} ${CFLAGS} $(filter %.o %.a,$^) ${LIBS} -o $@
	-rm -f $@.mod # $@.lnk.c $@.c

%: %.pl ${PLC1} ${PL_LIB}
	pl -x ${PLC1} -g "comp_file('$*.pl')" -t halt -L8192

%.mod: %.pl ${PLC1}
	pl -x ${PLC1} -g "comp_file('$*.pl')" -t halt -L8192
########################################################################
