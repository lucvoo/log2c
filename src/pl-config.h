/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#ifndef PL_CONFIG_H__
#define PL_CONFIG_H__

//#define TIME_OF_DAY
//#define DEBUG_STACKS

#define SP_IN_REG
#define FP_IN_REG
#define HP_IN_REG
// #define BTP_IN_REG
// #define TP_IN_REG

#define	WORDS_LITTLEENDIAN
#define	CELL_BITS	32	// in bits
#define	CELL_SIZE	4	// in bytes

#define TAG_BITS	3
#define TAG_POS		29	// (CELL_BITS-TAG_BITS)
#define TAG_MASK	(7<<29)	// (((1<<TAG_BITS)-1)<<TAG_POS)
#define VAL_MASK	(~TAG_MASK)
#define GC_BITS		2


#define PL_MAX_TAG_INT	(VAL_MASK>>1)
#define PL_MAX_INT	PL_MAX_TAG_INT
#define PL_MIN_TAG_INT	(-(PL_MAX_TAG_INT+1))
#define PL_MIN_INT	(-(PL_MAX_INT+1))

#define MAX_RING_BUFFER	4



// FIXME : should be done by configure & C°
#define	PL_ARCH		"i486-linux"
#define PL_HOME		"/home/luc/log"
#define PL_VERSION	809
#define CC		"gcc"
#define C_LD_FLAGS	""
#define C_LIBS		""
#define C_STATIC_LIBS	""
#define C_OPTIONS	"-fno-builtin -funsigned-char -O2 -ggdb -ffixed-%ebx -ffixed-%esi -ffixed-%edi"


#define HAVE_REMOVE
#define HAVE_REALPATH
#define HAVE_ASPRINTF
#define HAVE_PIPE

typedef unsigned long pl_word_t;

#endif // PL_CONFIG_H__