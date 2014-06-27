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
	errmsg.pl \
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
	\
	plc.pl


GEN     = pl-ext.pl
TRASH	= ${GEN} ${PLC}
CLEAN	= ${TRASH}

all: all_plc

include ../Make.common

all_plc: ${PLC}

########################################################################

${PLC}: ${SRC} pl-ext.pl
	pl -o $@ -c plc.pl

pl-ext.pl: ${src_dir}/pl-ext.def ${src_dir}/pl-ext.h
	@echo "GEN	$@"
	@echo "%% Automatically generated from $<"	> $@
	@echo "%% !!! DO NOT EDIT !!!"			>> $@
	@echo						>> $@
	@echo ":- module('$*', [foreign_pred_builtin/3])." >> $@
	@echo						>> $@
	@gcc -E -P -x c-header -DPL ${src_dir}/pl-ext.h	>> $@

########################################################################
# Dependencies

${src_dir}/pl-ext.h: ${src_dir}/pl-ext.def

