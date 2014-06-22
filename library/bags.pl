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
	'$findall_start',
	findall_recording(V, G),
	'$findall_collect'(B), !.

findall_recording(V, G) :-
	call(G),
	'$findall_record'(V),
	fail.
findall_recording(_, _).
