/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module(map_name, [
		map_C_op/2,
		map_atom/2,
		map_fun/2,
		map_pred/3
	]).


map_atom(A, N) :-
	'$mangle'(A, N).

map_fun(F/N, Fm) :-
	map_atom(F, Na),
	concat_atom([Na, '_', N], Fm).

map_pred(F, M, P) :-
	map_fun(F, Fm),
	map_atom(M, Mm),
	concat_atom(['_', Mm, Fm], P).

map_C_op(@<, <).
map_C_op(@>, >).
map_C_op(@=<, <=).
map_C_op(@>=, >=).
map_C_op(==, ==).
map_C_op(\==, '!=').
map_C_op(<, <).
map_C_op(>, >=).
map_C_op(=<, <=).
map_C_op(>=, >=).
map_C_op(=:=, ==).
map_C_op(=\=, '!=').
