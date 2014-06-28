/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module('$apply', [
		foldl/4,
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


:- meta_predicate sublist(1, +, ?).
sublist(G, [A|X], R) :-
	call(G, A, B), !,
	R=[B|Y],
	sublist(G, X, Y).
sublist(G, [_|X], R) :-
	sublist(G, X, R).
sublist(_, [], []).
