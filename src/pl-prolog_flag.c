/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "Prolog.h"
#include "pl-status.h"
#include "pl-atom.h"

struct pl_status PL__status;

#define	pl_flags_size	16

enum prolog_flag_type {
	T_AUTO = -1,
	T_NEW,
	T_ATOM,
	T_INTG,
	T_BOOL,
};


struct prolog_flag {
	struct atom *key;
	union {
		struct atom *atom;
		long intg;
	} val;
	union {
		int *intg;
		struct atom **atom;
	} addr;
	int lock;
	enum prolog_flag_type type;
	struct prolog_flag *next;
};

static struct prolog_flag *pl_flags[pl_flags_size];


static struct prolog_flag *pflag_lookup(struct atom *key, int new)
{
	struct prolog_flag *f;
	hash_t h;

	h = key->hash % pl_flags_size;

	for (f = pl_flags[h]; f; f = f->next)
		if (key == f->key)
			return f;	// find flag.

	if (new) {
		f = NEW(*f);		// create new flag
		f->key = key;		// with this keys.
		f->lock = 0;
		f->addr.atom = 0;
		f->type = T_NEW;
		f->next = pl_flags[h];	// insert this flag in the table
		pl_flags[h] = f;
		return f;
	} else
		return 0;		// inexistant flag
}


static int pf_check_type(const struct prolog_flag *f, int type, int t)
{
	return ((type == T_AUTO || type == t) && (f->type == T_NEW || f->type == t));
}

static void pf_set_new_type(struct prolog_flag *f, int type)
{
	if (f->type == T_NEW) {
		f->type = type;
	}
}

static int pf_set_atom(struct prolog_flag *f, struct atom *val)
{
	if (f->addr.atom)
		*f->addr.atom = val;
	else
		f->val.atom = val;

	succeed;
}

static int pf_set_new_atom(struct prolog_flag *f, struct atom *val)
{
	pf_set_new_type(f, T_ATOM);
	return pf_set_atom(f, val);
}

static int pf_new_atom(const char *key, struct atom *val, int lock, struct atom **addr)
{
	struct prolog_flag *f = pflag_lookup(PL_new_atom(key), 1);

	f->lock = lock;
	f->type = T_ATOM;
	if (!addr)
		addr = &f->val.atom;
	f->addr.atom = addr;
	return pf_set_atom(f, val);
}

static int pf_new__str(const char *key, const char *val, int lock, struct atom **addr)
{
	return pf_new_atom(key, PL_new_atom(val), lock, addr);
}

static int pf_set_int(struct prolog_flag *f, long val)
{
	if (f->addr.intg)
		*f->addr.intg = val;	// int *!
	else
		f->val.intg = val;	// long !

	succeed;
}

static int pf_set_new_bool(struct prolog_flag *f, long val)
{
	pf_set_new_type(f, T_BOOL);
	return pf_set_int(f, val);
}

static int pf_set_new_intg(struct prolog_flag *f, long val)
{
	pf_set_new_type(f, T_INTG);
	return pf_set_int(f, val);
}

static int pf_new_int(const char *key, long val, int lock, int *addr, int type)
{
	struct prolog_flag *f = pflag_lookup(PL_new_atom(key), 1);

	f->lock = lock;
	f->type = type;
	f->addr.intg = addr;
	return pf_set_int(f, val);
}

static int pf_new_intg(const char *key, long val, int lock, int *addr)
{
	return pf_new_int(key, val, lock, addr, T_INTG);
}

static int pf_new_bool(const char *key, long val, int lock, int *addr)
{
	return pf_new_int(key, val, lock, addr, T_BOOL);
}


int pl_set_prolog_flag(union cell *key, union cell *new)
{
	struct prolog_flag *f;
	struct atom *k;

	if (!(k = PL_get_atom(key)))
		fail;

	if (!(f = pflag_lookup(k, 0)))
		fail;

	if (f->lock)
		fail;			// flag is locked

	new = deref(new);
	switch (get_tag(new)) {
	case int_tag:
		if (f->type == T_INTG)
			return pf_set_int(f, get_val(new));
		fail;
	case ato_tag:
		if (f->type == T_BOOL) {
			if (isatom(ATOM(_true), new) || isatom(ATOM(_on), new)) {
				return pf_set_int(f, 1);
			} else if (isatom(ATOM(_false), new) || isatom(ATOM(_off), new)) {
				return pf_set_int(f, 0);
			}
		}
		if (f->type == T_ATOM)
			return pf_set_atom(f, get_atom(new));
	default:
		fail;
	}
}

static int PL_unify_prolog_flag(struct prolog_flag *f, union cell *term)
{
	switch (f->type) {
	case T_ATOM:
		return PL_unify_atom(term, f->addr.atom ? *f->addr.atom : f->val.atom);

	case T_BOOL:
		return PL_unify_bool(term, f->addr.intg ? *f->addr.intg : f->val.intg);

	case T_INTG:
		return PL_unify_intg(term, f->addr.intg ? *f->addr.intg : f->val.intg);

	default:
		fail;
	}
}

int pl_prolog_flag(union cell *key, union cell *val)
{
	struct prolog_flag *f;
	struct atom *k;

	if (!(k = PL_get_atom(key)))
		fail;

	if (!(f = pflag_lookup(k, 0)))
		fail;

	if (f->type == T_NEW)
		fail;

	if (!PL_unify_prolog_flag(f, val))
		fail;

	succeed;
}

int pl_current_prolog_flag(union cell *key, union cell *val, enum control *ctrl)
{
	struct prolog_flag *f;
	unsigned int h;
	struct {
		unsigned int h;
		struct prolog_flag *f;
	} *ctxt;

	switch (GetCtrl(ctrl)) {
	case FIRST_CALL:
		if (pl_prolog_flag(key, val))
			succeed;

		if (!is_var(key))
			fail;

		ctxt = AllocCtxt(*ctxt);
		h = 0;
		f = pl_flags[h];
		break;
	case NEXT_CALL:
		ctxt = GetCtxt(ctrl);
		h = ctxt->h;
		f = ctxt->f;
		break;
	default:
		fail;
	}

	for (; h < pl_flags_size; f = pl_flags[++h]) {
		for (; f; f = f->next) {
			if (f->type == T_NEW)
				continue;
			if (PL_unify_prolog_flag(f, val)) {
				PL_put_atom(key, f->key);
				ctxt->h = h;
				ctxt->f = f->next;
				if (!f->next && h == (pl_flags_size-1)) // last element of last slot
					succeed;
				retry;
			}
		}
	}

	fail;				// Should nerver be reached
}

// #####################################################################

extern int PL_create_prolog_flag(union cell *key, union cell *val, int lock, int type, int keep)
{
	struct prolog_flag *f;
	struct atom *k;


	if (!(k = PL_get_atom(key)))
		fail;

	if (!(f = pflag_lookup(k, 1)))
		fail;

	if (keep && f->type != T_NEW)
		succeed;		// the flag already exists: we keep it
	if (f->lock)
		fail;			// the flag already exist and is locked/not changeable
	if (f->type == T_NEW)
		f->lock = lock;

	val = deref(val);
	switch (get_tag(val)) {
	case ato_tag:
		if (isatom(ATOM(_true), val) || isatom(ATOM(_on), val)) {
			if (pf_check_type(f, type, T_BOOL))
				return pf_set_new_bool(f, 1);
		} else if (isatom(ATOM(_false), val) || isatom(ATOM(_off), val)) {
			if (pf_check_type(f, type, T_BOOL))
				return pf_set_new_bool(f, 0);
		}

		if (!pf_check_type(f, type, T_ATOM))
			fail;
		return pf_set_new_atom(f, get_atom(val));

	case int_tag:
		if (!pf_check_type(f, type, T_INTG))
			fail;
		return pf_set_new_intg(f, get_intg(val));
	default:
		fail;
	}
}


#include "pl-option.h"

static const struct pl_option_map map_lock[] = {
	{ ATOM(_read__write), 0, },
	{ ATOM(_read__only),  1, },
	{ NULL }
};
static const struct pl_option_map map_type[] = {
	{ ATOM(_boolean), T_BOOL, },
	{ ATOM(_integer), T_INTG, },
	{ ATOM(_atom),    T_ATOM, },
//	{ ATOM(_float),   T_FLOAT, },
//	{ ATOM(_term),    T_TERM, },
	{ NULL }
};
static long opt_lock;
static long opt_type;
static int opt_keep;
static struct pl_option_spec specs[] = {
	{ ATOM(_access),	OPT_ATOMS, .val.intg = &opt_lock, .map = map_lock, },
	{ ATOM(_type),		OPT_ATOMS, .val.intg = &opt_type, .map = map_type, },
	{ ATOM(_keep),		OPT_BOOL,  .val.bool = &opt_keep, },
	{ NULL }
};

int pl_create_prolog_flag(union cell *key, union cell *val, union cell *options)
{
	opt_lock = 0;
	opt_type = T_AUTO;
	opt_keep = 0;

	if (!PL_scan_options(options, specs))
		PL_warning("create_prolog_flag/3 : Illegal option list");

	return PL_create_prolog_flag(key, val, opt_lock, opt_type, opt_keep);
}

// #####################################################################

#include "pl-init.h"
#include <unistd.h>

void PL_init_prolog_flag(void)
{
/* ISO prolog-flags */
	pf_new_intg("bounded", 1, 1, 0);
	pf_new_bool("char_conversion", 0, 0, &PL__status.char_conv);
	pf_new_bool("debug", 0, 0, &PL__status.debug);
	pf_new__str("double_quotes", "codes", 0, &PL__status.dbl_quotes);
	pf_new__str("integer_rounding_function", (-3 / 2) == -2 ? "down" : "toward_zero", 1, 0);
	pf_new_intg("max_arity", PL_MAX_INT, 1, 0);
	pf_new_intg("max_integer", PL_MAX_INT, 1, 0);
	pf_new_intg("min_integer", PL_MIN_INT, 1, 0);
	pf_new__str("unknown", "fail", 1, 0);

	pf_new__str("back_quotes", "codes", 0, &PL__status.bck_quotes);

/* SWI flags (please, features, unknow, style_check, fileerrors) */
	pf_new_intg("address_bits", sizeof(void *)*8, 1, 0);
#if defined(__APPLE__)
	pf_new_bool("apple", 1, 1, 0);
#endif
	pf_new__str("arch", PL_ARCH, 1, 0);
//	pf_new__str("c_cc", CC, 1, 0);
//	pf_new__str("c_ldflags", C_LD_FLAGS, 1, 0);
//	pf_new__str("c_libs", C_LIBS, 1, 0);
//	pf_new__str("c_options", C_OPTIONS, 1, 0);
//	pf_new__str("c_staticlibs", C_STATIC_LIBS, 1, 0);
//	pf_new_bool("open_shared_object", 0, 1, 0);
	pf_new_bool("character_escapes", 1, 0, &PL__status.char_esc);
	pf_new__str("compiled_at", __DATE__ ", " __TIME__, 1, 0);
	pf_new_bool("dynamic_stacks", 1, 1, 0);
	pf_new__str("float_format", "%g", 0, &PL__status.float_fmt);
	pf_new_bool("gc", 0, 1, 0);
	pf_new__str("home", PL_HOME, 1, 0);
	pf_new_bool("iso", 0, 1, &PL__status.iso);
	pf_new_intg("max_tagged_integer", PL_MAX_TAG_INT, 1, 0);
	pf_new_intg("min_tagged_integer", PL_MIN_TAG_INT, 1, 0);
	pf_new_intg("pid", getpid(), 1, 0);
	pf_new_bool("pipe", 1, 1, 0);
	pf_new_bool("readline", 0, 1, 0);
	pf_new_bool("report_error", 1, 0, &PL__status.rep_err);
	pf_new_bool("tty_control", 1, 0, &PL__status.tty_ctrl);
#if defined(__unix__) || defined(unix) || defined(__APPLE__)
	pf_new_bool("unix", 1, 1, 0);
#endif
	pf_new_intg("version", PL_VERSION, 1, 0);

/* SWI flags : style_check */
	pf_new_bool("discontiguous", 0, 0, &PL__status.discont);
	pf_new_bool("dollar", 0, 0, &PL__status.dollar);
	pf_new_bool("singleton", 1, 0, &PL__status.singleton);

/* SWI flags : fileerrors */
	pf_new_bool("file_error", 0, 0, &PL__status.file_err);

/* Own extension */
	pf_new_intg("nested_comment", 1, 0, &PL__status.nested_com);
}
