/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module('$bags', [
		findall/4,
		findall/3
	]).


%% FIXME: doesn't seems to work in general (but seems to work well for findall/3)
findall(V, G, B, T) :-
	'$findall_start',
	findall_recording(V, G),
	'$findall_collect'(B, T), !.

findall(V, G, B) :-
	findall(V, G, B, []).

findall_recording(V, G) :-
	call(G),
	'$findall_record'(V),
	fail.
findall_recording(_, _).
