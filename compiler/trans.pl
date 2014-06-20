/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module(trans, [
		trans/2,
		trans_term/2
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fun(T, F, N, A) :-
	compound(T),
	functor(T, F, N),
	T=..[F|A].

%%%%%%%%
trans_term((A, B), (X, Y)) :-
	trans_term(A, X),
	trans_term(B, Y).
trans_term((A;B), (X;Y)) :-
	trans_term(A, X),
	trans_term(B, Y).
trans_term((A| B), (X| Y)) :-
	trans_term(A, X),
	trans_term(B, Y).
trans_term((A->B), (X->Y)) :-
	trans_term(A, X),
	trans_term(B, Y).
trans_term((A*->B), (X*->Y)) :-
	trans_term(A, X),
	trans_term(B, Y).
trans_term(not(A), not(X)) :-
	trans_term(A, X).
trans_term(\+A, \+X) :-
	trans_term(A, X).
trans_term(A, X) :-
	A=..[F|La],
	maplist(trans:trans, La, Lx),
	X=..[F|Lx].

%%%%%%%%
trans(A, X) :-
	atom(A),
	X=atom(A).
trans(A, X) :-
	integer(A),
	X=intg(A).
trans(A, X) :-
	float(A),
	X=flt(A).
trans(A, A) :-
	var(A).
trans(T, X) :-
	fun(T, F, N, A),
	maplist(trans:trans, A, At),
	X=fun(F, N, At).
