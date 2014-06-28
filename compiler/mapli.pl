/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module(mapli, [
		mapli/4,
		mapli/5
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- meta_predicate mapli(+, 4, +, ?), mapli(+, 4, +, ?, ?).

mapli(N, G, I, L) :-
	mapli(N, G, I, L, []).

mapli(_, _, [], L, L).
mapli(N, G, [E|Q], I, O) :-
	succ(N, M),
	call(G, M, E, I, T),
	mapli(M, G, Q, T, O).

