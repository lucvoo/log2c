########################################################################
##

src_dir	= ../src

SRC = \
	addr.pl \
	arith.pl \
	atoms.pl \
	aux.pl \
	builtin.pl \
	code.pl \
	comp.pl \
	hash.pl \
	map_name.pl \
	modules.pl \
	my_dcg.pl \
	swi.pl \
	trad.pl \
	trans.pl \
	util.pl \
	vars.pl \
	var_util.pl \


GEN     = pl-ext.pl
TRASH	= ${GEN} ${PLC}
CLEAN	= ${TRASH}

all: all_plc

include ../Make.common

all_plc: ${PLC}

########################################################################

${PLC}: ${SRC} pl-ext.pl
	pl -o $@ -c comp.pl

pl-ext.pl: ${src_dir}/pl-ext.def ${src_dir}/pl-ext.h
	echo ":- module('$*',[ foreign_pred_builtin/4 ])." > $@
	echo >> $@
	gcc -E -P -x c-header -DPL ${src_dir}/pl-ext.h | grep -v "^[ ]*$$" >> $@

########################################################################
# Dependencies

${src_dir}/pl-ext.h: ${src_dir}/pl-ext.def

