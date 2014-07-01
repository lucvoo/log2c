/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module(aux, [
		export_pred/1,
		read_all/2,
		read_export/2
	]).

:- use_module(errmsg).
:- use_module(foreign).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Read all terms of file 'F' and put it in list 'L'

read_all(F, L) :-
	current_input(Old),
	open(F, read, N),
	set_input(N),
	read_all_(L),
	close(N),
	set_input(Old).

read_all_(L) :-
	read(T),
	(   
		T==end_of_file
	->
		L=[]
	;
		read_all_(Q),
		L=[Tx|Q],
		expand_term(T, Tx)
	).

read_export(F, X) :-
	current_input(Old),
	open(F, read, N),
	set_input(N),
	read_x_(X),
	close(N),
	set_input(Old).

read_x_(X) :-
	read(T),
	(   
		T==end_of_file
	->
		fail
	;
		
		T=export(X)
	->
		true
	;
		read_x_(X)
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
export_pred(Xs) :-
	format(mod, 'export(~q).\n', [Xs]),
	map_recorda(export_pred, Xs).
map_recorda(_, []).
map_recorda(K, [A|Q]) :-
	recorda(K, A),
	map_recorda(K, Q).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
