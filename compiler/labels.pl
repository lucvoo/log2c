/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module(labels, [
		del_labels/0,
		getlabel/2,
		getlabel1/3,
		label/2,
		label/3
	]).

:- use_module(map_name).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
del_labels :-
	current_flag(K),
	atom(K),
	concat(label_, _, K),
	flag(K, _, 0),
	fail.
del_labels.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

label(F, A, Label) :-
	map_fun(F/A, Fm),
	label(Fm, Label).
label(Fm, Label) :-
	concat(Fm, '_', Base),
	concat(label_, Base, Key),
	flag(Key, Old, Old+1),
	succ(Old, New),
	concat(Base, New, Label).
getlabel(Fm, L) :-
	concat(Fm, '_', Base),
	concat(label_, Base, Key),
	flag(Key, N, N),
	concat(Base, N, L).
getlabel1(F, A, L) :-
	map_fun(F/A, Fm),
	concat(Fm, '_', Base),
	concat(label_, Base, Key),
	flag(Key, N, N),
	succ(N, M),
	concat(Base, M, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
