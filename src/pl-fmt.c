/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "Prolog.h"
#include "pl-ctype.h"
#include "pl-stream.h"
#include "pl-fli.h"
#include "pl-io.h"
#include "pl-write.h"

#define BUFSIZE 	10240
#define DEFAULT 	(-1)
#define SHIFT   	{ ++argv; }
#define NEED_ARG	{ if (!argv->celp) \
			  { ERROR("not enough arguments"); \
			  } \
			}
#define ERROR(fmt)	PL_warning("format/2: " fmt)
#define ERROR1(fmt, a)	PL_warning("format/2: " fmt, a)

#define OUTSTRING(s)	Sputs(S,s)
#define OUTCHR(c)	Sputc(S,c)

		/********************************
		*       UTILITIES		*
		********************************/

static char digitname[] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
	'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
	'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
	'u', 'v', 'w', 'x', 'y', 'z'
};

static char DigitName[] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
	'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
	'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
	'U', 'V', 'W', 'X', 'Y', 'Z'
};

inline static char digitName(int n, bool small)	// FIXME : check if overflow ???
{
	return (small ? digitname[n] : DigitName[n]);
}

		/********************************
		*       ACTUAL FORMATTING	*
		********************************/

/* format an integer according to a number of modifiers at various
   radius. `split' is a boolean asking to put ',' between each group
   of three digits (e.g. 67,567,288). `div' askes to divide the number
   by radix^`div' before printing. `radix' is the radix used for
   conversion. `n' is the number to be converted.

 ** Fri Aug 19 22:26:41 1988  jan@swivax.UUCP (Jan Wielemaker)
*/
static char *formatInteger(bool split, int div, int radix, bool small, long int n)
{
	static char tmp[100];
	char *s = tmp + 99;
	int before = (div == 0);
	int digits = 0;
	bool negative = FALSE;

	*s = EOS;
	if (n < 0) {
		n = -n;
		negative = TRUE;
	}
	if (n == 0 && div == 0) {
		*--s = '0';
		return s;
	}
	while (n > 0 || div >= 0) {
		if (div-- == 0 && !before) {
			*--s = '.';
			before = 1;
		}
		if (split && before && (digits++ % 3) == 0 && digits != 1)
			*--s = ',';
		*--s = digitName((int)(n % radix), small);
		n /= radix;
	}
	if (negative)
		*--s = '-';

	return s;
}

inline static int update_column(int col, int c)
{
	switch (c) {
	case '\n':
		return 0;
	case '\t':
		return (col + 1) | 0x7;
	case '\b':
		return (col <= 0 ? 0 : col - 1);
	default:
		return col + 1;
	}
}

static bool do_format(const char *fmt, union cell *argv, pl_stream S)
{
	while (*fmt) {
		if (*fmt == '~') {
			int arg = DEFAULT;	/* Numeric argument */
			/* Get the numeric argument */
			if (isDigit(*++fmt)) {
				for (; isDigit(*fmt); fmt++)
					arg = (arg == DEFAULT ? *fmt - '0' : arg * 10 + *fmt - '0');
			} else if (*fmt == '*') {
				NEED_ARG;
				if (PL_get_integer(argv, &arg) && arg >= 0) {
					SHIFT;
				} else
					ERROR("no or negative integer for `*' argument");
				fmt++;
			} else if (*fmt == '`') {
				arg = *++fmt;
				fmt++;
			}

			switch (*fmt) {	/* Build in formatting */
			case 'a':{
					const char *s;	/* Atomic */

					NEED_ARG;
					if (!PL_get_chars(argv, &s, CVT_ATOMIC))
						ERROR("illegal argument to ~a");
					SHIFT;
					Sputs(S, s);
					fmt++;
					break;
				}
			case 'c':{
					int c;	/* ascii */

					NEED_ARG;
					if (PL_get_integer(argv, &c) && c >= 0 && c <= 255) {
						int times = (arg == DEFAULT ? 1 : arg);

						SHIFT;
						while (times-- > 0) {
							OUTCHR(c);
						}
					} else
						ERROR("illegal argument to ~c");
					fmt++;
					break;
				}
			case 'd':	/* integer */
			case 'D':	/* grouped integer */
			case 'r':	/* radix number */
			case 'R':	/* Radix number */
				{
					int i;
					char *s;

					NEED_ARG;
					if (!PL_get_integer(argv, &i))
						ERROR1("illegal argument to ~%c", *fmt);
					SHIFT;
					if (arg == DEFAULT)
						arg = 0;
					if (*fmt == 'd' || *fmt == 'D')
						s = formatInteger(*fmt == 'D', arg, 10, TRUE, i);
					else
						s = formatInteger(FALSE, 0, arg, *fmt == 'r', i);

					Sputs(S, s);
					fmt++;
					break;
				}
			case 's':	/* string */
				{
					const char *s;

					NEED_ARG;
					if (!PL_get_list_codes(argv, &s, BUF_DISCARDABLE))
						ERROR("illegal argument to ~s");
					Sputs(S, s);
					SHIFT;
					fmt++;
					break;
				}
			case 'i':	/* ignore */
				{
					NEED_ARG;
					SHIFT;
					fmt++;
					break;
				}
				{
					int (*f) (pl_stream, union cell *);
			case 'k':	/* displayq */
					f = PL_displayq;
					goto pl_common;
			case 'q':	/* writeq */
					f = PL_writeq;
					goto pl_common;
			case 'w':	/* write */
					f = PL_write;

pl_common:				NEED_ARG;
					(*f) (S, argv);
					SHIFT;
					fmt++;
					break;
				}
			case '~':	/* ~ */
				{
					OUTCHR('~');
					fmt++;
					break;
				}
			case 'n':	/* \n */
			case 'N':	/* \n if not on newline */
				{
					if (arg == DEFAULT)
						arg = 1;
					// if ( *fmt == 'N' && column == 0 )  arg--;
					while (arg-- > 0)
						OUTCHR('\n');
					fmt++;
					break;
				}
			}
		} else
//    if (*fmt=='%')
//    {
//    }
//    else
		{
			OUTCHR(*fmt);
			fmt++;
		}
	}

	succeed;
}

static union cell end_cell = {.celp = 0 };

static union cell *empty_tab = &end_cell;

inline static union cell *list_to_tab(union cell *list)
{
	int n = 0;
	union cell *l;

	l = deref(list);
	while (is_cons(l)) {
		HP[n].celp = l + 1;
		l = deref(l + 2);
		n++;
	}
	if (!is_nil(l)) {
		HP[0].celp = list;
		n = 1;
	}
	HP[n] = end_cell;
	return (HP);
}

int pl_format(union cell *fmt, union cell *args)
{
	const char *f;

	if (!PL_get_chars(fmt, &f, CVT_ALL | BUF_RING))
		PL_warning("format/2: format is not an atom or string");

	return (do_format(f, list_to_tab(args), PL_OutStream()));
}

int pl_format3(union cell *stream, union cell *fmt, union cell *args)
{
	const char *f;
	pl_stream S = PL_Output_Stream(stream);

	if (!PL_get_chars(fmt, &f, CVT_ALL | BUF_RING))
		PL_warning("format/2: format is not an atom or string");

	return (do_format(f, list_to_tab(args), S));
}

int pl_sformat3(union cell *string, union cell *fmt, union cell *args)
{
	const char *f, *s;
	int rval;
	pl_stream S = Sopen_wmem(0, SM_WRITE, 0);

	if (!PL_get_chars(fmt, &f, CVT_ALL | BUF_RING)) {
		Sclose(S);
		PL_warning("format/2: format is not an atom or string");
	}

	rval = do_format(f, list_to_tab(args), S);
	s = Sstring_wmem(S);
	Sclose(S);
	if (!rval)
		fail;
	else
		return (PL_unify_atom_chars(string, s));
}

int pl_sformat2(union cell *string, union cell *fmt)
{
	const char *f, *s;
	int rval;
	pl_stream S = Sopen_wmem(0, SM_WRITE, 0);

	if (!PL_get_chars(fmt, &f, CVT_ALL | BUF_RING)) {
		Sclose(S);
		PL_warning("format/2: format is not an atom or string");
	}

	rval = do_format(f, empty_tab, S);
	s = Sstring_wmem(S);
	Sclose(S);
	if (!rval)
		fail;
	else
		return (PL_unify_atom_chars(string, s));
}

int pl_int_to_atom2(union cell *num, union cell *atom)
{
	static char buf[100];		// Always large enough to store an 32 bit int
	long int n;
	char *s = buf + sizeof(buf);

	if (!PL_get_long(num, &n))
		PL_warning("int_to_atom/2: instantiation fault");

	*--s = '\0';
	if (n == 0)
		*--s = '0';
	else
		for (; n > 0; n /= 10)
			*--s = digitName(n % 10, 1);

	return (PL_unify_atom_chars(atom, s));
}

int pl_int_to_atom3(union cell *num, union cell *base, union cell *atom)
{
	static char buf[100];		// Always large enough to store an 32 bit int
	long int n;
	int b;
	char *s = buf + sizeof(buf);

	if (!PL_get_long(num, &n) || !PL_get_integer(base, &b))
		PL_warning("int_to_atom/3: instantiation fault");

	*--s = '\0';

	if (b == 0 && n > 0 && n < 256) {
		*--s = n;		// FIXME : what to do with non-printable char ?
		*--s = '\'';
		*--s = '0';
	} else if (b > 36 || b < 2)
		PL_warning("int_to_atom/3: Illegal base: %d", b);
	else {				// write the number
		if (n == 0)
			*--s = '0';
		else
			for (; n > 0; n /= b)
				*--s = digitName(n % b, 1);

		// write the base if necessary
		if (b != 10) {
			*--s = '\'';
			for (; b > 0; b /= 10)
				*--s = digitName(b % 10, 1);
		}
	}

	return (PL_unify_atom_chars(atom, s));
}
