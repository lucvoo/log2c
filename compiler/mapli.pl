/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module(mapli, [
		mapli/3,
		mapli/4
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- meta_predicate mapli(4, +, ?), mapli(4, +, ?, ?).

mapli(G, I, O) :-
	'$mapli'(I, G, 0, O, []).

mapli(G, I, O, T) :-
	'$mapli'(I, G, 0, O, T).


:- meta_predicate '$mapli'(+, 4, +, ?, ?).

'$mapli'([], _, _, T, T).
'$mapli'([E|I], G, N, O, T) :-
	succ(N, M),
	call(G, M, E, O, OT),
	'$mapli'(I, G, M, OT, T).

