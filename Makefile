########################################################################
##

src_dir	 = src
inc_dir	 = ${src_dir}
comp_dir = compiler
sub_dir	 = ${src_dir} ${comp_dir}

include	Make.common

PL_LIB	= ${src_dir}/${PLLIB}
PL_C	= ${comp_dir}/${PLC}
PLS	= $(wildcard *.pl)
TRASH	= $(PLS:%.pl=%.{c,o,mod,lnk.c}) $(PLS:%.pl=%)
CLEAN	= ${TRASH}

########################################################################
clean::
	touch ChangeLog
########################################################################
.PHONY: FORCE

FORCE: ;

${PL_LIB}: FORCE
	@${MAKE} -C ${src_dir} $(notdir $@)
${PL_C}: FORCE
	@${MAKE} -C ${comp_dir} $(notdir $@)

${COMP}: $*.pl

${PROG}: ${MODULES:%=%.o} ${PROG}.pl ${PROG}.o ${PROG}.lnk.o ${PROG}.mod ${PL_LIB} 
	${CC} ${CFLAGS} $(filter %.o %.a,$^) -o $@
	-rm -f $@.mod # $@.lnk.c $@.c

%: %.pl ${PL_C} ${PL_LIB}
	pl -x ${PL_C} -g "comp_file('$*.pl')" -t halt
%.mod: %.pl ${PL_C}
	pl -x ${PL_C} -g "comp_file('$*.pl')" -t halt
########################################################################
# Dependencies
system.mod: library/list.pl library/bags.pl
