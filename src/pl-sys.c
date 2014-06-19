/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "Prolog.h"
#include "pl-os.h"
#include "pl-fli.h"
#include "pl-atom.h"
#include "pl-init.h"			// For PL_init_argv()
#include <sys/time.h>

int pl_shell(union cell *command, union cell *status)
{
	const char *cmd;

	if (!PL_get_atom_chars(command, &cmd))	// FIXME : use PL_get_chars ?
		PL_warning("shell/2: instantiation fault");
	else
		return PL_unify_intg(status, PL_System(cmd));
}

int pl_get_time(union cell *time)
{
	struct timeval tv;

	gettimeofday(&tv, 0);
	return PL_unify_flt(time, tv.tv_usec / 1e6 + tv.tv_sec);
}

int pl_convert_time(union cell *Time, union cell *Year, union cell *Month, union cell *Day, union cell *Hour, union cell *Minute,
		    union cell *Second, union cell *Milli)
{
	double t;

	if (PL_get_flt(Time, &t)) {
		long s = (long)t;
		long ms = (long)((t - s) * 1000);
		struct tm *tm;

		tm = PL_LocalTime(s);
		return (PL_unify_intg(Year, tm->tm_year + 1900) &&
			PL_unify_intg(Month, tm->tm_mon + 1) &&
			PL_unify_intg(Day, tm->tm_mday) &&
			PL_unify_intg(Hour, tm->tm_hour) &&
			PL_unify_intg(Minute, tm->tm_min) &&
			PL_unify_intg(Second, tm->tm_sec) && PL_unify_intg(Milli, ms));
	} else
		PL_warning("convert_time/8: instantiation fault");
}

int pl_getenv(union cell *var, union cell *val)
{
	const char *n;

	if (PL_get_chars(var, &n, CVT_ALL)) {
		char *v;

		if ((v = getenv(n)))
			return PL_unify_atom_chars(val, v);
		else
			fail;
	} else
		PL_warning("getenv/2: instantiation fault");
}

int pl_setenv(union cell *var, union cell *val)
{
	const char *n, *v;

	if (PL_get_chars(var, &n, CVT_ALL | BUF_RING) && PL_get_chars(val, &v, CVT_ALL)) {
		return PL_setenv(n, v) != 0;
	} else
		PL_warning("setenv/2: instantiation fault");
}

int pl_unsetenv(union cell *var)
{
	const char *n;

	if (PL_get_chars(var, &n, CVT_ALL)) {
		PL_unsetenv(n);
		succeed;
	} else
		PL_warning("unsetenv/1: instantiation fault");
}

static union cell *argv;

void PL_init_argv(int arg_c, char **arg_v)
{
	union cell *a;

	a = argv = SHP;
	SHP += (2 * arg_c + 1);

	while (arg_c--) {
		a[0].val = __cons();
		a[1].val = __atom(PL_new_atom(*arg_v++));
		a += 2;
	}
	a[0].val = __nil();
}

int pl_argv(union cell *a)
{
	return pl_unify(argv, a);
}
