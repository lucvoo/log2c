/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module(util, [
		add_module/4,
		anf_module/3,
		decl_atoms/1,
		decl_export_mod/1,
		decl_funs/1,
		decl_import_mod/1,
		decl_pred/1,
		decl_preds/1,
		find_fvar/2,
		get_preds/2,
		get_query/3,
		include_module/1,
		init_jmp_tbl/2,
		init_preds/1,
		make_ARGs/2,
		make_f_args/2,
		meta_pred/2
	]).

:- use_module(aux).
:- use_module([map_name, atoms, modules]).
:- use_module(errmsg).

:- op(1200, xfx, :+).
:- op(900, fy, +>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_f_args(N, A) :-
	make_f_args_(N, A).
make_f_args_(0, '') :- !.
make_f_args_(N, A) :-
	M is N-1,
	make_f_args_(M, B),
	N4 is N+4,
	concat_atom([B, 'FP[', N4, '].celp, '], A).
		   
make_ARGs(0, '') :- !.
make_ARGs(1, 'PL_ARG(1)') :- !.
make_ARGs(N, A) :-
	M is N-1,
	make_ARGs(M, B),
	concat_atom([B, ', PL_ARG(', N, ')'], A).
		   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

find_fvar(G, L) :-
	to_list(G, Lg),
	maplist(util:find_fv, Lg, Lv),
	flatten(Lv, Lf),
	sort(Lf, L).

find_fv(G, L) :-
	G=..[_|Lg],
	maplist(util:find_fv_, Lg, L).

find_fv_(G, L) :-
	find_fv_(G, L, []).
find_fv_(E) :+
	E=var(f, _),
	+> E.
find_fv_(E) :+
	E=var(ft, _),
	+> E.
find_fv_(E) :+
	E=fun(_, _, A),
	maplist(util:find_fv_, A, L),
	+> L.
find_fv_(_) :+
	true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_preds(L, Lp) :-
	flag(max_tmp, _, 0),
	sortCls_(L, [], Lp), !.

sortCls_([], L, L).
sortCls_([C|Q], I, O) :-
	getC(C, Ci),
	max_tmp(C),
	sortCls_(Q, I, IO),
	insertC(Ci, IO, O).


getC((H:-B), cl(F, N, A, B)) :- !,
	fun(H, F, N, A).
getC((:-main(Q, V)), q(V, Q)) :- !.
getC((:-D), '') :- !,
	(
		do_directive(D)
	->
		true
	;
		fun(D, F, N, _),
		warning('unknow directive : ~w/~w', [F, N])
	).
getC(H, cl(F, N, A, true)) :-
	fun(H, F, N, A).

insertC('', L, L).
insertC(q(V, Q), I, O) :-
	select(q(Lq), I, Is), !,
	O=[q([cl(V, Q)|Lq])|Is].
insertC(q(V, Q), I, O) :-
	O=[q([cl(V, Q)])|I].
insertC(cl(F, N, A, B), I, O) :-
	select(pr(F, N, Lc), I, Is), !,
	O=[pr(F, N, [cl(A, B)|Lc])|Is].
insertC(cl(F, N, A, B), I, O) :-
	O=[pr(F, N, [cl(A, B)])|I].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_query(I, Q, O) :-
	select(q(Lq), I, O),
	check_query(Lq, Q), !.
get_query(_, _, _) :-
	fatal('no query given').

check_query([cl(_, Q)], Q).
check_query(_L, _Q) :-
	fatal('several queries given').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

anf_module(La, Lf, Lp) :-
	flag(current_module, M, M),
	module_filename(h, M, H),
	format('#include <Prolog.h>\n'),
	format('#include <pl-trad.h>\n\n', []),
	format('#include "~w"~n~n', [H]),
	map_atom(M, Mm),
	format(h, '#ifndef MODULE~w_H_\n', [Mm]),
	format(h, '#define MODULE~w_H_\n\n', [Mm]),
	used_modules(Ms),
	maplist(util:include_module, Ms),
	nl,
	decl_atoms(La),
	decl_funs(Lf),
	init_preds(Lp),
	init_args.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_args :-
	nl,
	flag(max_tmp, T, T),
	(
		between(1, T, I),
		format('union cell *TMP_~w;\n', [I]),
		fail
	;
		true
	),
	nl.

init_preds([F|Q]) :-
	recorda(preds, F),
	map_pred(F, Pm),
	format(h, 'extern unsigned int PRED~w;\n', Pm),
	init_preds(Q).
init_preds([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
decl_export_mod(export(X, L)) :-
	flag(current_module, M, M),
	maplist(util:decl_exp_mod(M, X), L).

decl_exp_mod(M, X, P) :-
	map_pred(P, M, Pm),
	map_pred(P, X, Px),
	format(h, '#define PRED~w PRED~w~n', [Pm, Px]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decl_preds(X) :-
	'$recorded_all'(export_pred, P),
	append(X, P, T),
	sort(T, L),
	maplist(util:decl_pred, L),
	nl.

decl_pred(P) :-
	import_from_module(P, M),
	map_pred(P, M, Pm),
	format(h, 'extern unsigned int PRED~w;\n', Pm).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decl_atoms(As) :-
	maplist(util:decl_atoms_, As),
	nl(h).
decl_atoms_(A) :-
	map_atom(A, Am),
	format(h, 'extern struct atom ATOM_~w;\n', [Am]),
	format(mod, '~q.\n', [atoms(A)]).

decl_funs(Fs) :-
	maplist(util:decl_funs_, Fs),
	nl(h).

decl_funs_(F/N) :-
	map_atom(F, Fm),
	format(h, 'extern struct functor FUN_~w_~d;\n', [Fm, N]),
	format(mod, '~q.\n', [funs(F/N)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_jmp_tbl(import, M) :-
	map_atom(M, Mm),
	format('&module~w, ', Mm).

init_jmp_tbl(pub, FN) :-
	\+ exported(FN), !.
init_jmp_tbl(_, F/N) :-
	map_atom(F, Fm),
	format('{ FUN(~w,~d), &&~w_~d_1}, ', [Fm, N, Fm, N]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decl_import_mod(M) :-
	map_atom(M, Mm),
	format('extern struct module module~w;\n', Mm).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

max_tmp(T) :-
	term_variables(T, L),
	length(L, N),
	flag(max_tmp, O, max(O, N)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_module(M, I, A, Arg) :-
	add_mod_(1, I, M, A, Arg).

add_mod_(_, _, _, [], []).
add_mod_(I, I, M, [A|X], [fun(:, 2, [atom(M), A])|X]) :- !.
add_mod_(I, N, M, [A|X], [A|Y]) :-
	succ(I, J),
	add_mod_(J, N, M, X, Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_directive(export(_)) :-
	warning('unsupported directive : use module/2').
do_directive(import(M)) :-
	do_directive(use_module(M)).
do_directive(import(M, L)) :-
	do_directive(use_module(M, L)).
do_directive(index(_)).
		%% FIXME : to implement
do_directive((meta_predicate P)) :-
	recordz(meta_pred, meta_pred(P)),
	recordz(meta, P).
do_directive(module(_)) :-
	warning('unsupported directive: module/1\n\tuse module/2').
do_directive((module_transparent _)) :-
	warning('unsupported directive: module_transparent/1\n\t\tuse ISO meta_predicate/1').
do_directive(op(P, T, N)) :-
	op(P, T, N).
do_directive(reexport(M)) :-
	recorda(export_module, M),
	recorda(use_module, M).
do_directive(reexport(M, _)) :-
	do_directive(reexport(M)).
do_directive(foreign(_, _)).
	%% FIXME : to implement
do_directive(foreign(P)) :-
	do_directive(foreign(P, [])).
do_directive(use_module(M)) :-
	recorda(use_module, M).
do_directive(use_module(M, _)) :-
	do_directive(use_module(M)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
meta_pred(maplist/2, 1).
meta_pred(maplist/3, 1).
meta_pred(findall/4, 2).
meta_pred(findall/3, 2).
meta_pred(bagof/3, 2).
meta_pred(setof/3, 2).
meta_pred(mapi/3, 2).
meta_pred(foldl/4, 1).
meta_pred(mapli/3, 1).
meta_pred(mapli/4, 1).
meta_pred(call/1, 1).
% Useless as this one is inlined
meta_pred(P, I) :-
	recorded(meta_pred, meta_pred(P, I)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
include_module(M) :-
	module_filename(h, M, H),
	format('#include "~w"\n', [H]).
