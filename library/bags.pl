/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module('$bags', [
		findall/3
	]).


findall(V, G, B) :-
	findall_recording(v-V, G),
	'$findall_collect'(B), !.

findall_recording(V, G) :-
	'$findall_record'(mark),
	call(G),
	'$findall_record'(V),
	fail.
findall_recording(_, _).
