/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module(export, [
		export_add/1,
		export_get/1
	]).

:- use_module(errmsg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

valid_PI(F,N) :-
	atom(F),
	integer(N),
	N >= 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export_add(Xs) :-
	ground(Xs), !,
	add_xs(Xs).
export_add(Xs) :-
	error('Instantiation error in ":- module(..., ~w)"',  [Xs]).

add_xs([]).
add_xs([PI|Q]) :-
	add_x(PI),
	add_xs(Q).
add_xs(PI) :-
	add_x(PI).


add_x(F/N) :-
	valid_PI(F,N),
	recordz(exported_pred, F/N).
add_x(F//N) :-
	valid_PI(F,N),
	N2 is N+2,
	recordz(exported_pred, F/N2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export_get(Xs) :-
	'$recorded_all'(exported_pred, Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
