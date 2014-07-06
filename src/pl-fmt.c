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

#define DEFAULT 	-1

#define GET_NEXT_ARG(argv)	\
({	union cell *arg;	\
	argv = deref(argv);	\
	if (!is_cons(argv))	\
		PL_warning("%s:%d: not enough arguments", __FUNCTION__, __LINE__); \
	arg = argv + 1;		\
	argv = argv + 2;	\
	arg;			\
})

/************************************************************************/

inline static char digit_name(unsigned int n, int small)	// FIXME : check if overflow ???
{
	if (n < 10)
		return n + '0';
	if (small)
		return n + 'a' - 10;
	else
		return n + 'A' - 10;
}

/************************************************************************/
/* Actual formatting

   Format an integer according to a number of modifiers at various radius.
   - `split' is a boolean asking to put ',' between each group of three digits (e.g. 67,567,288).
   - `div' askes to divide the number by radix^`div' before printing.
   - `radix' is the radix used for conversion.
   - `n' is the number to be converted.

 ** Fri Aug 19 22:26:41 1988  jan@swivax.UUCP (Jan Wielemaker)
*/
static void format_integer(struct stream *S, long int n, int split, int div, int radix, int small)
{
	char tmp[100];
	char *s = tmp + 99;
	int before = (div == 0);
	int digits = 0;
	int negative = FALSE;

	*s = '\0';
	if (n < 0) {
		n = -n;
		negative = TRUE;
	}
	if (n == 0 && div == 0) {
		*--s = '0';
		goto end;
	}
	while (n > 0 || div >= 0) {
		if (div-- == 0 && !before) {
			*--s = '.';
			before = 1;
		}
		if (split && before && (digits++ % 3) == 0 && digits != 1)
			*--s = ',';
		*--s = digit_name(n % radix, small);
		n /= radix;
	}
	if (negative)
		*--s = '-';

end:
	Sputs(S, s);
}

inline static int update_column(int col, int c)
{
	switch (c) {
	case '\n':
		return 0;
	case '\t':
		return (col + 1) | 0x7;
	case '\b':
		return col <= 0 ? 0 : col - 1;
	default:
		return col + 1;
	}
}

static int do_format(struct stream *S, const char *fmt, union cell *argv)
{
	unsigned int c;
#if 0
fprintf(stderr, "do_format(\"%s\") with ", fmt);
PL_write(Stderr, argv);
fprintf(stderr, "\n");
#endif
	while ((c = *fmt++)) {
		union cell *arg;
		int narg;	// numeric argument

		if (c != '~') {
			Sputc(S, c);
			continue;
		}

		// process the numerical argument, if present
		narg = DEFAULT;
		c = *fmt++;
		if (isDigit(c)) {
			narg = 0;
			do {
				narg = narg * 10  + c -'0';
			} while (isDigit((c = *fmt++)));
		} else if (c == '*') {
			arg = GET_NEXT_ARG(argv);
			if (!PL_get_intg(arg, &narg))
				PL_warning("format/2,3: no integer for `*' argument");
			else if (narg >= 0)
				PL_warning("format/2,3: negative integer for `*' argument");
			c = *fmt++;
		} else if (c == '`') {
			narg = *fmt++;
			c = *fmt++;
		}

		switch (c) {	// Build in formatting
			int (*f) (struct stream *, union cell *);
			const char *s;
			int ch;
			int i;

		case 'a':
			arg = GET_NEXT_ARG(argv);
			if (!PL_get_chars(arg, &s, CVT_ATOMIC))
				PL_warning("format/2,3: illegal argument to ~a");
			Sputs(S, s);
			break;

		case 'c':
			arg = GET_NEXT_ARG(argv);
			if (PL_get_intg(arg, &ch) && ch >= 0 && ch <= 255) {
				int times = (narg == DEFAULT ? 1 : narg);

				while (times-- > 0)
					Sputc(S, ch);
			} else
				PL_warning("format/2,3: illegal argument to ~c");
			break;

		case 'd':	// integer normal
		case 'D':	// integer grouped
		case 'r':	// integer radix
		case 'R':	// integer radix uppercase
			arg = GET_NEXT_ARG(argv);
			if (!PL_get_intg(arg, &i))
				PL_warning("format/2,3: illegal argument to ~%c", c);
			if (narg == DEFAULT)
				narg = 0;
			if (c == 'd' || c == 'D')
				format_integer(S, i, c == 'D', narg, 10, TRUE);
			else
				format_integer(S, i, FALSE, 0, narg, c == 'r');
			break;

		case 's':	// string
			arg = GET_NEXT_ARG(argv);
			if (!PL_get_list_codes(arg, &s, BUF_DISCARDABLE))
				PL_warning("format/2,3: illegal argument to ~s");
			Sputs(S, s);
			break;

		case 'i':	// ignore
			arg = GET_NEXT_ARG(argv);
			break;

		case 'k':	// write_canonical
			f = PL_displayq;
			goto formated_write;
#if 0	// TODO
		case 'p':	// print
			f = PL_print;
			goto formated_write;
#endif
		case 'q':	// writeq
			f = PL_writeq;
			goto formated_write;
		case 'w':	// write
			f = PL_write;

formated_write:
			arg = GET_NEXT_ARG(argv);
			f(S, arg);
			break;

		case '~':	// ~
			Sputc(S, '~');
			break;

		case 'n':	// \n
		case 'N':	// \n if not on newline
			if (narg == DEFAULT)
				narg = 1;
			// if ( *fmt == 'N' && column == 0 )  narg--;
			while (narg-- > 0)
				Sputc(S, '\n');
			break;

		default:
			PL_warning("format/2,3: illegal format ~%c", c);
		}
	}

	succeed;
}

void PL_format(const char *fmt, union cell *arg)
{
	union cell argv[3] = {
		[0].val = __cons(),
		[1].celp = arg,
		[2].val = __nil(),
	};

	do_format(Stderr, fmt, argv);
}


int pl_format(union cell *fmt, union cell *args)
{
	struct stream *S = PL_OutStream();
	const char *f;

	if (!PL_get_chars(fmt, &f, CVT_ALL | BUF_RING))
		PL_warning("format/2: format is not an atom or string");

	return do_format(S, f, args);
}

int pl_format3(union cell *stream, union cell *fmt, union cell *args)
{
	struct stream *S = PL_Output_Stream(stream);
	const char *f;

	if (!PL_get_chars(fmt, &f, CVT_ALL | BUF_RING))
		PL_warning("format/2: format is not an atom or string");

	return do_format(S, f, args);
}

int pl_sformat3(union cell *string, union cell *fmt, union cell *args)
{
	const char *f, *s;
	int rval;
	struct stream *S = Sopen_wmem(0, SM_WRITE, 0);

	if (!PL_get_chars(fmt, &f, CVT_ALL | BUF_RING)) {
		Sclose(S);
		PL_warning("format/2: format is not an atom or string");
	}

	rval = do_format(S, f, args);
	s = Sstring_wmem(S);
	Sclose(S);
	if (!rval)
		fail;
	else
		return PL_unify_atom_chars(string, s);
}

int pl_sformat2(union cell *string, union cell *fmt)
{
	const char *f, *s;
	int rval;
	struct stream *S = Sopen_wmem(0, SM_WRITE, 0);

	if (!PL_get_chars(fmt, &f, CVT_ALL | BUF_RING)) {
		Sclose(S);
		PL_warning("format/2: format is not an atom or string");
	}

	rval = do_format(S, f, new_atom(ATOM(nil)));
	s = Sstring_wmem(S);
	Sclose(S);
	if (!rval)
		fail;
	else
		return PL_unify_atom_chars(string, s);
}

int pl_int_to_atom2(union cell *num, union cell *atom)
{
	char buf[100];		// Always large enough to store an 32 bit int
	long int n;
	char *s = buf + sizeof(buf);

	if (!PL_get_long(num, &n))
		PL_warning("int_to_atom/2: instantiation fault");

	*--s = '\0';
	if (n == 0)
		*--s = '0';
	else
		for (; n > 0; n /= 10)
			*--s = digit_name(n % 10, 1);

	return PL_unify_atom_chars(atom, s);
}

int pl_int_to_atom3(union cell *num, union cell *base, union cell *atom)
{
	char buf[100];		// Always large enough to store an 32 bit int
	long int n;
	int b;
	char *s = buf + sizeof(buf);

	if (!PL_get_long(num, &n) || !PL_get_intg(base, &b))
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
				*--s = digit_name(n % b, 1);

		// write the base if necessary
		if (b != 10) {
			*--s = '\'';
			for (; b > 0; b /= 10)
				*--s = digit_name(b % 10, 1);
		}
	}

	return PL_unify_atom_chars(atom, s);
}
