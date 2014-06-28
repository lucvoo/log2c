/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module(swi, [
		'$erase_records'/1,
		'$functor'/4,
		'$mangle'/2,
		'$recorded_all'/2,
		hpjw/2
	]).



'$recorded_all'(K, L) :-
	findall(R, recorded(K, R), L).

'$erase_records'(K) :-
	recorded(K, _, R),
	erase(R),
	fail.
'$erase_records'(_).



'$mangle'(A, Ma) :-
	atom_codes(A, L),
	c_id(L, Ml), !,
	atom_codes(Ma, [95|Ml]).

c_id([], []) :- !.
c_id([95|Q], [95, 95|Mq]) :- !,
	c_id(Q, Mq).
c_id([C|Q], [C|Mq]) :-
	is_csym(C), !,
	c_id(Q, Mq).
c_id([C|Q], M) :-
	A is C//16,
	hex_digit(A, XA),
	B is C mod 16,
	hex_digit(B, XB),
	M=[95, XA, XB|Mq],
	c_id(Q, Mq).

hex_digit(V, D) :-
	between(0, 9, V),
	D is V+48.
hex_digit(V, D) :-
	between(10, 15, V),
	D is V+ (65-10).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hpjw(A, H) :-
	atom_codes(A, L),
	hpjw(L, 0, H).

hpjw([], H, H).
hpjw([C|L], S, H) :-
	H1 is S << 4 + C,
	G is H1 /\ 0xf0000000,
	(
		G =\= 0
	->
		H2 is H1 xor (G>>24) xor G
	;
		H2 = H1
	),
	hpjw(L, H2, H).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
'$functor'(T, F, N, As) :-
	functor(T, F, N),
	T =.. [F|As].
