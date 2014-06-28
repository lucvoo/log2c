/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module(var_util, [
		member_v/2,
		subtract_v/3
	]).


member_v(E, [F|_]) :-
	E==F.
member_v(E, [_|Q]) :-
	member_v(E, Q).

memberchk_v(E, [F|_]) :-
	E==F, !.
memberchk_v(E, [_|Q]) :-
	memberchk_v(E, Q).


subtract_v([E|Q], D, O) :-
	(
		memberchk_v(E, D)
	->
		O=Qo
	;
		O=[E|Qo]
	),
	subtract_v(Q, D, Qo).
subtract_v([], _, []).
