/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module(aux, [
		del_all/0,
		export_pred/1,
		exported/1,
		flag2/3,
		module_filename/3,
		read_all/2,
		read_export/2
	]).

:- use_module(errmsg).
:- use_module(foreign).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
del_all :-
	'$erase_records'(code),
	'$erase_records'(vars_list),
	'$erase_records'(directive),
	'$erase_records'(curr_C),
	'$erase_records'(indent),
	'$erase_records'(preds),
	'$erase_records'(export_pred),
	'$erase_records'(used_modules),
	'$erase_records'(module_compiled),
	flag(indent, _, 0).

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
flag2(K, V1, V2) :-
	(   
		var(V1),
		var(V2)
	->
		W1=V1,
		W2=V2
	;
		true
	),
	concat(K, '_1', K1),
	flag(K1, W1, V1),
	concat(K, '_2', K2),
	flag(K2, W2, V2).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
export_pred(Xs) :-
	format(mod, 'export(~q).\n', [Xs]),
	map_recorda(export_pred, Xs).
map_recorda(_, []).
map_recorda(K, [A|Q]) :-
	recorda(K, A),
	map_recorda(K, Q).

exported(P) :-
	recorded(export_pred, P).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%! module_base(+Module, -Basename) is det
%
% Return the basename corresponding to a module.
module_basename(user, B) :-
	!,
	flag(input_file, B, B).
module_basename(M, B) :-
	(
		concat($, R, M)
	->
		B = R
	;
		B = M
	).

%! module_filename(+Extension, +Module, -Basename) is det
%
% Return the filename corresponding to a module.
module_filename(X, M, F) :-
	module_basename(M, B),
	file_name_extension(B, X, F).
