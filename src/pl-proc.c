/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "Prolog.h"
#include "pl-fli.h"
#include "pl-write.h"

#ifndef HWREG_ARGS
union cell *PL_ARGS[PL_MAX_ARGS] = { 0 };
#else
union cell *PL_ARGS_[PL_MAX_ARGS] = { 0 };
#endif

extern struct modules *PL__modules[];
extern struct module module_system;
extern int PL__modules_hash_size;

inline static struct module *lookup_module(struct atom *name)
{
	hash_t h;
	struct modules *m;

	h = name->hash % PL__modules_hash_size;
	for (m = PL__modules[h]; m != 0; m = m->next)
		if (m->name == name)
			return m->module;

	return 0;
}

inline static void *lookup_proc(struct module * module, struct atom *functor, int arity)
{
	hash_t h;
	struct jmp *j;

	h = (functor->hash + arity) % module->all.size;
	for (j = module->all.tab[h]; j != 0; j = j->next) {
		if (j->functor == functor && j->arity == arity)
			return j->pred;
	}

// if fail : try default module : system        // FIXME
	module = &module_system;
	h = (functor->hash + arity) % module->pub.size;
	for (j = module->pub.tab[h]; j != 0; j = j->next) {
		if (j->functor == functor && j->arity == arity)
			return j->pred;
	}

	PL_write(Stderr, (union cell *) functor);
	PL_warning("%s/%d lookup_proc : no such procedure", functor->name, arity);
}

inline static union cell *strip_module(union cell *term, struct module ** module)
{
	union cell *m = 0;
	union cell *t;
	struct module *mod;

	t = deref(term);
	while (t->val == __fun(FUN(module, 2))) {
		m = t + 1;
		t = deref(t + 2);
	}

	if (!m)				// if no module given : try default module : system // FIXME
		m = new_atom(ATOM(_system));
	else
		m = deref(m);

	if (is_atom(m) && (mod = lookup_module(get_atom(m)))) {
		*module = mod;
		return deref(t);
	}

	PL_write(Stderr, term);
	PL_warning(" : strip_module : illegal module:term specification");
}

void *PL_call(union cell *clos, int extra, union cell ** args)
{
	union cell *t;
	struct atom *name;
	int arity, n;
	struct module *mod = 0;
	void *proc;

	if (!(t = strip_module(clos, &mod)) ||
	    !PL_get_name_arity(t, &name, &arity) || !(proc = lookup_proc(mod, name, arity + extra))) {
		PL_write(Stderr, clos);
		PL_warning("PL_call : fail");
	}

	for (n = 1; n <= arity; n++)
		PL_ARGS[n] = deref(t + n);
	for (n = 0; n < extra; n++)
		PL_ARGS[arity + 1 + n] = args[n];

	return proc;
}

void *PL_apply(union cell *clos, union cell *list)
{
	union cell *t;
	struct atom *name;
	int arity, n, extra;
	struct module *mod;
	void *proc;

	list = deref(list);

	if (!(t = strip_module(clos, &mod)) ||
	    !PL_get_name_arity(t, &name, &arity) ||
	    !((extra = PL_lengthList(list)) >= 0) || !(proc = lookup_proc(mod, name, arity + extra))) {
		PL_write(Stderr, clos);
		PL_warning("PL_apply : fail");
	}

	for (n = 1; n <= arity; n++)
		PL_ARGS[n] = deref(t + n);
	for (n = 0; n < extra; n++) {
		PL_ARGS[arity + 1 + n] = deref(list + 1);
		list = deref(list + 2);
	}

	return proc;
}
