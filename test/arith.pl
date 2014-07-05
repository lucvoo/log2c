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
		%format('OK: ~w == ~w\n', [Q, R]),
		true
	;
		format(user_error, 'KO: ~w != ~w\n', [Q, R])
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
case(0+0, 0).
case(0+1, 1).
case(1+0, 1).
case(1+1, 2).
case(1+2+3, 6).
case(N+1, M) :- between(0,10,N), succ(N,M).
case(N+M, R) :- some(N), some(M), plus(N,M, R).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
case(N//2, M)	:- some(M), N is M * 2.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
case(N << 0, N) :- some(N).
case(1 << N, M) :- powof2(N, M).
case(M << 1, P) :- powsof2(_, M, _, P).
case(2 << N, P) :- powsof2(N, _, _, P).

case(N >> 0, N) :- some(N).
case(N >> 1, M) :- some(N), M is N // 2.
case(M >> N, 1) :- powof2(N, M).
case(P >> 1, M) :- powsof2(_, M, _, P).
case(P >> N, 2) :- powsof2(N, _, _, P).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



/************************************************************************/
some(N) :- between(0, 32, N).
some(N) :- powof2(_, N).
some(N) :- member(N, [169, 65535]).		%% FIXME: should test a bunch of randoms

powof2(0, 1).
powof2(1, 2).
powof2(2, 4).
powof2(3, 8).
powof2(4, 16).
powof2(5, 32).
powof2(6, 64).
powof2(7, 128).
powof2(8, 256).
powof2(9, 512).
powof2(10, 1024).
powof2(11, 2048).
powof2(12, 4096).
powof2(13, 8192).
powof2(14, 16384).
powof2(15, 32768).
powof2(16, 65536).
powof2(17, 131072).
powof2(18, 262144).
powof2(19, 524288).
powof2(20, 1048576).
powof2(21, 2097152).
powof2(22, 4194304).
powof2(23, 8388608).
powof2(24, 16777216).
powof2(25, 33554432).
powof2(26, 67108864).
powof2(27, 134217728).

powsof2(N, M, O, P) :- powof2(N, M), succ(N, O), powof2(O, P).
