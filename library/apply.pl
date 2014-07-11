/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module('$apply', [
		foldl/4,
		exclude/3,
		include/3,
		maplist/2,
		maplist/3,
		sublist/3
	]).


:- meta_predicate foldl(3, +, +, -).
foldl(_, [], R, R).
foldl(G, [E|Q], I, O) :-
	call(G, E, I, T),
	foldl(Q, G, T, O).

:- meta_predicate maplist(1, ?).
maplist(G, [A|X]) :-
	call(G, A),
	maplist(G, X).
maplist(_, []).


:- meta_predicate maplist(2, ?, ?).
maplist(G, [A|X], [B|Y]) :-
	call(G, A, B),
	maplist(G, X, Y).
maplist(_, [], []).


:- meta_predicate include(1, +, ?).
include(G, [A|X], R) :-
	call(G, A), !,
	R=[A|Y],
	include(G, X, Y).
include(G, [_|X], R) :-
	include(G, X, R).
include(_, [], []).

:- meta_predicate sublist(1, +, ?).
sublist(G, I, O) :-
	include(G, I, O).


:- meta_predicate exclude(1, +, ?).
exclude(G, [A|X], R) :-
	call(G, A), !,
	exclude(G, X, R).
exclude(G, [A|X], R) :-
	R=[A|Y],
	exclude(G, X, Y).
exclude(_, [], []).
