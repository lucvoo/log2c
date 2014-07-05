/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

main :- test_all.

test_all :-
	case(Q,R), test(Q, R), fail.
test_all.

test(Q, R) :-
	A is Q,
	(
		A = R
	->
		format('OK: ~w == ~w\n', [Q, R]),
		true
	;
		format('KO: ~w != ~w\n', [Q, R])
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
case(0+0, 0).
case(0+1, 1).
case(1+0, 1).
case(1+1, 2).
case(N+1, M) :- between(0,10,N), succ(N,M).
case(N+M, R) :- some(N), some(M), plus(N,M, R).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



/************************************************************************/
some(N) :- between(0, 32, N).
some(N) :- member(N, [169, 65535]).		%% FIXME: should test a bunch of randoms
