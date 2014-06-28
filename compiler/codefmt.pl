/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module(codefmt, [
		comm/1,
		comm/2,
		comm/3,
		comm/4,
		comm/5,
		comm_pred/2,
		f/1,
		f/2,
		fl/1,
		fl_/1,
		g/1,
		g/2,
		g0/1,
		g0/2,
		new_indent/1
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map_name_v(var(_, N), M) :-
	concat('_var_', N, M).
map_name_v(atom(E), E).
map_name_v(intg(E), E).
map_name_v(flt(E), E).
%% map_name_v(string(E),E).
map_name_v(fun(F, _, A), M) :-
	maplist_map_name_v(A, X),
	M=..[F|X].
map_name_v(E, E) :-
	atomic(E).

maplist_map_name_v([], []).
maplist_map_name_v([A|X], [B|Y]) :-
	map_name_v(A, B),
	maplist_map_name_v(X, Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
new_indent(N) :-
	flag(indent, O, O+N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fl_(L) :-
	fl(L).
fl(L) :-
	format('~w:\n', [L]).
g(F, A) :-
	put(9),
	flag(indent, N, N),
	tab(N),
	format(F, A),
	nl.
g(F) :-
	g(F, []).
g0(F, A) :-
	format(F, A),
	nl.
g0(F) :-
	format(F),
	nl.
f(F, A) :-
	tab(9),
	flag(indent, N, N),
	tab(N),
	format(F, A).
f(F) :-
	f(F, []).


comm(H, A, B, C, D) :-
	map_name_v(A, Na),
	map_name_v(B, Nb),
	map_name_v(C, Nc),
	map_name_v(D, Nd),
	format('/* ~w(~w,~w,~w,~w) */\n', [H, Na, Nb, Nc, Nd]), !.
comm(H, A, B, C) :-
	map_name_v(A, Na),
	map_name_v(B, Nb),
	map_name_v(C, Nc),
	format('/* ~w(~w,~w,~w) */\n', [H, Na, Nb, Nc]), !.
comm(H, A, B) :-
	map_name_v(A, Na),
	map_name_v(B, Nb),
	format('/* ~w(~w,~w) */\n', [H, Na, Nb]), !.
comm(H, A) :-
	map_name_v(A, Na),
	format('/* ~w(~w) */\n', [H, Na]), !.
comm(H) :-
	format('/* ~w */\n', [H]), !.

comm_pred(F, N) :-
	format('/* code for ~w/~w */\n', [F, N]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
