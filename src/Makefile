########################################################################
##

include	../Make.common

inc_dir = .

SRC = \
	pl-arith.c \
	pl-atom.c \
	pl-buffer.c \
	pl-char.c \
	pl-ctype.c \
	pl-file.c \
	pl-flag.c \
	pl-fli.c \
	pl-fmt.c \
	pl-fun.c \
	pl-hash.c \
	pl-inline.c \
	pl-io.c \
	pl-iso.c \
	pl-list.c \
	pl-main.c \
	pl-network.c \
	pl-mangle.c \
	pl-op.c \
	pl-option.c \
	pl-os.c \
	pl-pred.c \
	pl-prims.c \
	pl-proc.c \
	pl-prolog_flag.c \
	pl-read.c \
	pl-rec.c \
	pl-stack.c \
	pl-stream.c \
	pl-Sfile.c \
	pl-Smem.c \
	pl-Spipe.c \
	pl-Sprintf.c \
	pl-Ssocket.c \
	pl-sys.c \
	pl-time.c \
	pl-util.c \
	pl-Ssocket.c \
	pl-write.c \


OBJS	= ${SRC:%.c=%.o}
DEPS	= ${SRC} ${filter %.c, ${OTHER_SRC} }
TRASH	= zz* *.s *.i .*.d
CLEAN	= ${TRASH} ${PLLIB} *.o

########################################################################

all: ${PLLIB}

${PLLIB}: ${OBJS}
	@ar rsu $@ ${OBJS}

########################################################################
# Dependencies

-include ${SRC:%.c=.%.d}
