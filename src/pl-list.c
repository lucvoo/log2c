/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "Prolog.h"
#include <stdlib.h>			// for qsort()


/**
  Calculate the length of the given list and return the tail,
  while doing a cycle detection.
  cfr. Brent's algorithm: http://en.wikipedia.org/wiki/Cycle_detection#Brent.27s_algorithm

  We can have the following, depending on te type of the tail:
  - []		proper list
  - Var		partial list
  - [_|_]	a cycle is detected, the returned length is meaningless
  - otherwise	malformed list

  @return the length of the proper part of the list
*/

int PL_list_tail(union cell *l, union cell **tail)
{
	unsigned long steps = 0;
	unsigned long limit = 2;
	union cell *turtle = 0;
	int n = 0;

loop:
	switch (get_tag(l)) {
	case ref_tag:
		l = l->celp;
		goto loop;
	case fun_tag:
		n++;

		steps++;
		if (l == turtle)	// the rabbit reach the turtle!
			break;		// -> cycle detected
		if (steps == limit) {
			limit *= 2;
			steps = 0;
			turtle = l;	// teleport the turtle
		}

		l = l + 2;
		goto loop;

	default:
		break;
	}

	*tail = l;
	return n;
}

int pl_is_list(union cell *l)
{
	return PL_is_cons(l);
}

int pl_proper_list(union cell *l)
{
	Deref(l);

	while (is_cons(l))
		l = deref(l + 2);

	return is_nil(l);
}

int pl_partial_list(union cell *l)
{
	Deref(l);

	while (is_cons(l))
		l = deref(l + 2);

	return is_var(l);
}

int pl_memberchk(union cell *e, union cell *l)
{
	Deref(l);
	Deref(e);

	while (is_cons(l)) {
		if (PL_try_unify(e, l + 1))
			succeed;
		l = deref(l + 2);
	}
	fail;
}

int pl_length(union cell *list, union cell *l)
{
	int m;
	Deref(list);

	if (PL_get_intg(l, &m)) {
		int n = m;

		while (is_cons(list) && n > 0) {
			n--;
			list = deref(list + 2);
		}

		if (n == 0 && is_nil(list))
			succeed;
		else if (!is_var(list) || n < 0)
			fail;
		else			// var(list) && n>= 0
		{
			union cell *c;
			list->celp = HP;
			trail(list);
			c = HP;
			HP += (2 * n);
			while (c < HP) {
				c[0].val = __cons();
				c[1].val = __var();
				c += 2;
			}		// POST : c==HP
			HP[0].val = __nil();
			HP++;
			succeed;
		}
	} else if (PL_is_var(l)) {
		int n = PL_lengthList(list);
		if (n >= 0)
			return PL_unify_intg(l, n);
	}

	fail;
}

/**********************************************************************/
/* Stuff for sort/2 and msort/2                                       */
/**********************************************************************/

// Return length of the list; -1 if not a proper_list
// Put the array in static_heap
inline static int list_to_array(union cell *list)
{
	union cell *l;
	int n = 0;

	l = deref(list);
	while (is_cons(l)) {
		(SHP++)->celp = l + 1;
		l = deref(l + 2);
		n++;
	}

	if (is_nil(l))
		return n;
	else
		return -1;
}

inline static union cell *array_to_list(union cell **array, int n, int rem_dup)
{
	union cell *l = HP;
	union cell *last;

	while (n--) {
		HP[0].val = __cons();
		HP[1].celp = last = *array++;
		HP += 2;

		if (rem_dup)
			while (n && pl_std_cmp(last, *array) == 0) {
				n--;
				array++;
			}
	}

	HP[0].val = __nil();
	HP++;
	return l;
}

inline static int PL_sort(union cell *list, union cell *sorted, int rem_dup)
{
	union cell **array = (union cell **) SHP;
	union cell *l;
	int n;

	n = list_to_array(list);
	SHP = (void *)array;

	if (n < 0)
		PL_warning("%s/2: first_argument is not a proper list", rem_dup ? "sort" : "msort");

	if (n != 0)
		qsort(array, n, sizeof(union cell *), pl_std_cmp);

	l = array_to_list(array, n, rem_dup);
	return pl_unify(l, sorted);
}

int pl_sort(union cell *list, union cell *sorted)
{
	return PL_sort(list, sorted, 1);
}

int pl_msort(union cell *list, union cell *sorted)
{
	return PL_sort(list, sorted, 0);
}
