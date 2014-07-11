/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module(util, [
		add_module/4,
		anf_module/4,
		decl_export_mod/1,
		decl_import_mod/1,
		decl_pred/1,
		decl_preds/1,
		find_fvar/2,
		get_preds/2,
		get_query/3,
		include_module/1,
		make_ARGs/2,
		make_f_args/2,
		meta_pred/2
	]).

:- use_module([map_name, modules]).
:- use_module(errmsg).
:- use_module(foreign).
:- use_module(atoms).

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
	maplist(find_fv, Lg, Lv),
	flatten(Lv, Lf),
	sort(Lf, L).

find_fv(G, L) :-
	G=..[_|Lg],
	maplist(find_fv_, Lg, L).

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
	maplist(find_fv_, A, L),
	+> L.
find_fv_(_) :+
	true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_preds(L, Lp) :-
	flag(max_tmp, _, 0),
	sort_clauses(L, [], Lp), !.

sort_clauses([], L, L).
sort_clauses([C|Q], I, O) :-
	get_clause(C, Ci),
	max_tmp(C),
	sort_clauses(Q, I, IO),
	insert_clause(Ci, IO, O).


get_clause((H:-B), cl(F, N, A, B)) :- !,
	'$functor'(H, F, N, A).
get_clause((:-main(Q, V)), q(V, Q)) :- !.
get_clause((:-D), '') :- !,
	(
		do_directive(D)
	->
		true
	;
		'$functor'(D, F, N, _),
		warning('unknow directive : ~w/~w', [F, N])
	).
get_clause(H, cl(F, N, A, true)) :-
	'$functor'(H, F, N, A).

insert_clause('', L, L).
insert_clause(q(V, Q), I, O) :-
	select(q(Lq), I, Is), !,
	O=[q([cl(V, Q)|Lq])|Is].
insert_clause(q(V, Q), I, O) :-
	O=[q([cl(V, Q)])|I].
insert_clause(cl(F, N, A, B), I, O) :-
	select(pr(F, N, Lc), I, Is), !,
	O=[pr(F, N, [cl(A, B)|Lc])|Is].
insert_clause(cl(F, N, A, B), I, O) :-
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

anf_module(M, P, Q, X) :-
	a_n_f(M, P, Q, X, La, Lf, Lp),
	module_filename(h, M, H),
	format('#include <Prolog.h>\n'),
	format('#include <pl-trad.h>\n\n', []),
	format('#include "~w"~n~n', [H]),
	map_atom(M, Mm),
	format(h, '#ifndef MODULE~w_H_\n', [Mm]),
	format(h, '#define MODULE~w_H_\n\n', [Mm]),
	used_modules(Ms),
	maplist(include_module, Ms),
	nl,
	decl_atoms(La),
	decl_funs(Lf),
	maplist(init_pred(M),Lp),
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

init_pred(M, F) :-
	recorda(preds, F),
	map_pred(F, M, Pm),
	format(h, 'extern unsigned int PRED~w;\n', Pm).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
decl_export_mod(export(X, L)) :-
	flag(current_module, M, M),
	maplist(decl_exp_mod(M, X), L).

decl_exp_mod(M, X, P) :-
	map_pred(P, M, Pm),
	map_pred(P, X, Px),
	format(h, '#define PRED~w PRED~w~n', [Pm, Px]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decl_preds(X) :-
	'$recorded_all'(export_pred, P),
	append(X, P, T),
	sort(T, L),
	maplist(decl_pred, L),
	nl.

decl_pred(P) :-
	import_from_module(P, M),
	map_pred(P, M, Pm),
	format(h, 'extern unsigned int PRED~w;\n', Pm).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decl_atoms(La) :-
	exclude(atoms, La, As),
	maplist(decl_atom, La), !,
	nl(h),
	format(mod, '\tatoms(~q),\n', [As]).

decl_atom(A) :-
	map_atom(A, Am),
	format(h, 'extern struct atom ATOM_~w;\n', [Am]).

decl_funs(Lf) :-
	exclude(functors, Lf, Fs),
	maplist(decl_fun, Lf), !,
	nl(h),
	format(mod, '\tfuns(~q)).\n', [Fs]).

decl_fun(F/N) :-
	map_atom(F, Fm),
	format(h, 'extern struct functor FUN_~w_~d;\n', [Fm, N]).

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
	recorda(use_module, M).		%% FIXME: ?
do_directive(reexport(M, _)) :-
	do_directive(reexport(M)).
do_directive(foreign(_, _)).
	%% FIXME : to implement
do_directive(foreign(P)) :-
	do_directive(foreign(P, [])).
do_directive(use_module(M)) :-
	do_use_module(M).
do_directive(use_module(M, Is)) :-
	do_use_module(M, Is).

do_use_module([]).
do_use_module([M|Q]) :-
	do_use_module(M, (*)),
	do_use_module(Q).
do_use_module(M) :-
	do_use_module(M, (*)).

do_use_module(M, Is) :-
	Is = _,				%% FIXME: should process the imports
	recorda(use_module, M).		%% FIXME: should compile the module if needed and then load the '.mod'

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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
to_list(T, R) :-
	to_list(T, R, []).

to_list(A, I, O) :-
	var(A), !,
	I=[A|O].
to_list((A, B), I, O) :-
	to_list(A, I, T),
	to_list(B, T, O).
to_list((A;B), I, O) :-
	to_list(A, I, T),
	to_list(B, T, O).
to_list((A| B), I, O) :-
	to_list(A, I, T),
	to_list(B, T, O).
to_list((A->B), I, O) :-
	to_list(A, I, T),
	to_list(B, T, O).
to_list((A*->B), I, O) :-
	to_list(A, I, T),
	to_list(B, T, O).
to_list(\+A, I, O) :-
	to_list(A, I, O).
to_list(not(A), I, O) :-
	to_list(A, I, O).
to_list(E, [E|O], O).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
a_n_f(M, G, Q, X, A, F, P) :-
	anf_rec_atom(M),
	anf_clause(cl([], Q)),
	maplist(anf_pred, G),
	maplist(anf_rec_fun, X),
	anf_foreign(M),
	anf(A, F, P).

anf_foreign(system) :-
	foreign_preds(Fps),
	maplist(anf_rec_pred, Fps).
anf_foreign(_).

anf(A, F, P) :-
	recorded(used_modules, Ms),
	maplist(anf_rec_atom, Ms),
	anf_rec_import,
	anf_get_atom(A),
	anf_get_fun(F),
	anf_get_pred(P).


anf_rec_import :-
	recorded(module_export, module_export(_, F/_)),
	anf_rec_atom(F),
	fail.
anf_rec_import.


anf_pred(pr(F, N, L)) :-
	anf_rec_pred(F/N),
	maplist(anf_clause, L).

anf_clause(cl(H, G)) :-
	maplist(anf_elem,H),
	to_list(G, L),
	maplist(anf_goal, L).

anf_goal(E) :-
	compound(E), !,
	%% We do not record the goal's atoms & functors, only the ones in their arguments
	E=..[_|L],
	maplist(anf_elem, L).
anf_goal(_).

anf_elem(E) :-
	compound(E), !,
	functor(E, F, N),
	anf_rec_fun(F, N),
	E=..[F|L],
	maplist(anf_elem, L).
anf_elem(E) :-
	atom(E), !,
	anf_rec_atom(E).
anf_elem(_).


anf_rec_atom(A) :-
	recorda(anf_rec_atom, A).

anf_rec_fun(F/N) :-
	anf_rec_fun(F, N).
anf_rec_fun(F, N) :-
	recorda(anf_rec_fun, F/N),
	anf_rec_atom(F).	%% FIXME: Needed? can be done at collect time

anf_rec_pred(P) :-
	recorda(anf_rec_pred, P),
	anf_rec_fun(P),		%% FIXME: Why de we really need that?
	true.


anf_get_erase(K, S) :-
	'$recorded_all'(K, L),
	sort(L, S),
	'$erase_records'(K).

anf_get_atom(A) :-
	anf_get_erase(anf_rec_atom, A).
anf_get_fun(F) :-
	anf_get_erase(anf_rec_fun, F).
anf_get_pred(P) :-
	anf_get_erase(anf_rec_pred, P).

