/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module(modules, [
		check_export/3,
		check_import/2,
		check_module/1,
		export_user_preds/1,
		exported/1,
		get_exports/2,
		import_from_module/2,
		load_mod/1,
		module_filename/3,
		need_modules/1,
		read_mods/4,
		used_modules/1
	]).

:- use_module(foreign).
:- use_module(errmsg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
export_pred(Xs) :-
	format(mod, '\texport(~q),\n', [Xs]),
	maplist(recorda(export_pred), Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_export(X1, L, Xm) :-
	flag(current_module, M, M),
	maplist(check_export1(M, L), X1),
	(
		M==system
	->
		foreign_preds(X2),
		append(X1, X2, X3)
	;
		X3=X1
	),
	export_l(Xm, Xl, X3),
	sort(Xl, Xs),
	export_pred(Xs).

check_export1(M, L, F/N) :-
	(
		memberchk(pr(F, N, _), L)
	->
		true
	;
		warning('Exported predicate: ~w:~w/~w is not defined', [M, F, N])
	).

export_l([], L, L).
export_l([M|Q], I, O) :-
	export_l_(M, I, N),
	export_l(Q, N, O).
export_l_(export(_, L), I, O) :-
	append(L, O, I).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export_user_preds(pr(F, N, _)) :-
	recorda(export_pred, F/N),
	recorda(module_export, module_export(user, F/N)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_mod(F, T) :-
	open(F, read, S),
	read(S, T),
	close(S).

read_mods(M, A, F, P) :-
	'$erase_records'(need_module),
	read_mod(M, A, [], F, [], P, []).

read_mod([], A, A, F, F, P, P).
read_mod([M|Q], Ia, Oa, If, Of, Ip, Op) :-
	read_mod(M, Ia, Ta, If, Tf, Ip, Tp),
	read_mod(Q, Ta, Oa, Tf, Of, Tp, Op).
read_mod(M, Ia, Oa, If, Of, Ip, Op) :-
	(
		recorded(need_module, M)
	->
		Ia=Oa,
		If=Of,
		Ip=Op
	;
		recorda(need_module, M),
		comp_sub_module(M, F),
		read_mod(F, T),
		T = module(Nm, use_module(Ms), export(Xs), ops(_), atoms(As), funs(Fs)),
		Nm = _, %% FIXME: verify Nm
		append(Xs, Tp, Ip),
		append(As, Ta, Ia),
		append(Fs, Tf, If),
		read_mod(Ms, Ta, Oa, Tf, Of, Tp, Op)	%% FIXME: why???
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load_mod([M|Q]) :-
	load_mod(M),
	load_mod(Q).
load_mod([]).
load_mod(M)	:- 			%% FIXME: should take a file spec as argument
	comp_sub_module(M, _).

comp_sub_module(M, P) :-
	recorded(module_compiled, M),
	module_path(M, P).
comp_sub_module(M, F) :-
	flag(input_file, I, I),
	M=I,
	module_path(M, F).
comp_sub_module(M, F) :-
	module_path(M, F),
	format(user_error, '[ Compiling module ~w :', M),
	concat_atom(['make ', F], Make),
	shell(Make, R),
	(
		R=0
	->
		Res=done
	;
		Res=failed,
		flag(error, E, E+1)
	),
	format(user_error, ' ~w ]\n', [Res]),
	recorda(module_compiled, M).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
module_path(M, F) :-
	module_filename('.mod', M, F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
used_modules(L) :-
	export_use_(use_module, used_modules, L).
exported_modules(L) :-
	export_use_(export_module, exported_modules, L).

export_use_(_, S, L) :-
	recorded(S, L), !.
export_use_(T, S, L) :-
	'$recorded_all'(T, Lm),
	flatten(Lm, B),
	sort(B, L),
	recorda(S, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_export(F, X) :-
	read_mod(F, module(_, _, export(X), _, _, _)).

get_exports(Lu, Lx) :-
	used_modules(U),
	exported_modules(X),
	maplist(get_xlist, U, Xs),
	maplist(prefix_export(Xs, use), U, Lu),
	maplist(prefix_export(Xs, export), X, Lx).

get_xlist(M, A) :-
	comp_sub_module(M, F), !,
	read_export(F, X),
	A=M-X.

prefix_export(L, P, M, R) :-
	memberchk(M-Xs, L),
	R=..[P, M, Xs].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
need_modules(Ms) :-
	'$recorded_all'(need_module, Ms).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_import(U, _X) :-
	'$erase_records'(module_export),
	used_modules(Us),
	format(mod, '\tuse_module(~q),\n', [Us]),
	maplist(rec_x, U).

rec_x(use(M, L)) :-
	maplist(rec_export(M), L).

rec_export(M, X) :-
	(
		recorded(module_export, module_export(Mm, X))
	->
		X=F/N,
		error('Predicate ~w/~w already imported from module ~w', [F, N, Mm])
	;
		recorda(module_export, module_export(M, X))
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
import_from_module(F, M) :-
	recorded(module_export, module_export(M, F)).
import_from_module(_, M) :-
	flag(current_module, M, M).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_module(P) :-
	recorded(module_export, module_export(_, P)), !.
check_module(P) :-
	recorded(preds, P), !.
check_module(F/N) :-
	foreign_pred(F/N, _, ndet).
check_module(F/N) :-
	(
		recorded(undef_pred, undef_pred(F, N))
	->
		true
	;
		recorda(undef_pred, undef_pred(F, N)),
		error('Undefined predicate: ~w/~w', [F, N])
	).
	%% further compilation will fail but
	%% we want message for other possible errors
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
