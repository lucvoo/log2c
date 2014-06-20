/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module('$grammar', [
		expand_term/2
	]).

:- use_module('$list').


expand_term((H-->G), (Th:-Tg)) :- !,
	tr_head(H, Th, I, O),
	dcg_tr(I, O, G, Tg).
expand_term((H:+G), (Th:-Tg)) :- !,
	tr_head(H, Th, I, O),
	dcg_tr_(I, O, G, Tg).
expand_term(G, G).


tr_head(H, Th, I, O) :-
	H=..Lh,
	append(Lh, [I, O], Lth),
	Th=..Lth.


dcg_tr(I, O, V, T) :-
	var(V), !,
	T=call(V, I, O).
dcg_tr(I, O, (G1, G2), T) :- !,
	dcg_tr(I, N, G1, T1),
	dcg_tr(N, O, G2, T2),
	T= (T1, T2).
dcg_tr(I, O, (G1->G2), T) :- !,
	dcg_tr(I, N, G1, T1),
	dcg_tr(N, O, G2, T2),
	T= (T1->T2).
dcg_tr(I, O, (G1;G2), T) :- !,
	dcg_tr(I, O, G1, T1),
	dcg_tr(I, O, G2, T2),
	T= (T1;T2).
dcg_tr(I, O, \+G, T) :- !,
	dcg_tr(I, O, G, Tg),
	T= (\+Tg).
dcg_tr(I, O, {G}, T) :- !,
	T= (G, O=I).
dcg_tr(I, O, [], T) :- !,
	T= (I=O).
dcg_tr(I, O, [E], T) :- !,
	T='C'(I, E, O).
%% FIXME : can inline
dcg_tr(I, O, [E|Q], T) :-
	proper_list([E|Q]), !,
	T=append([E|Q], O, I).
dcg_tr(I, O, =(E), T) :- !,
	T= (E=I, I=O).
dcg_tr(I, O, (G1| G2), T) :- !,
	dcg_tr(I, O, (G1;G2), T).
dcg_tr(I, O, not(G), T) :- !,
	dcg_tr(I, O, \+G, T).

dcg_tr(I, O, G, T) :-
	G=..L,
	append(L, [I, O], Lt),
	T=..Lt.





dcg_tr_(I, O, V, T) :-
	var(V), !,
	T=V.
dcg_tr_(I, O, (G1, G2), T) :- !,
	dcg_tr_(I, N, G1, T1),
	dcg_tr_(N, O, G2, T2),
	T= (T1, T2).
dcg_tr_(I, O, (C->G1;G2), T) :- !,
	dcg_tr_(I, N, G1, T1),
	dcg_tr_(N, O, G2, T2),
	T= (C->T1, N=O;I=N, T2).
dcg_tr_(I, O, (G1;G2), T) :- !,
	dcg_tr_(I, N, G1, T1),
	dcg_tr_(N, O, G2, T2),
	T= (T1, N=O;I=N, T2).
dcg_tr_(I, O, (G1->G2), T) :- !,
	dcg_tr_(I, N, G1, T1),
	dcg_tr_(N, O, G2, T2),
	T= (T1->T2).
dcg_tr_(I, O, \+G, T) :- !,
	dcg_tr_(I, O, G, Tg),
	T= (\+Tg).
dcg_tr_(I, O, +>E, T) :- !,
	T= (I=[E|O]).
dcg_tr_(I, O, (G1| G2), T) :- !,
	dcg_tr_(I, O, (G1;G2), T).
dcg_tr_(I, O, not(G), T) :- !,
	dcg_tr_(I, O, \+G, T).

dcg_tr_(I, O, G, T) :-
	functor(G, F, N),
	(
		is_dcg(F, N)
	->
		G=..L,
		append(L, [I, O], Lt),
		T=..Lt
	;
		I=O,
		T=G
	).



is_dcg(code_Pr, 1).
is_dcg(code__Pr, 1).
is_dcg(code_FPr, 0).
is_dcg(code_FPr_ndet, 1).
is_dcg(code_FPr_det, 1).
is_dcg(code_G, 1).
is_dcg(code_G, 2).
is_dcg(code_G_or, 4).
is_dcg(code_call, 2).
is_dcg(code_C, 4).
is_dcg(code_Arg, 2).
is_dcg(code_Offset, 2).
is_dcg(code_Addr, 3).
is_dcg(code_Addr, 2).
is_dcg(code_Assign, 2).
is_dcg(code_AssignD, 2).
is_dcg(code_U, 2).
is_dcg(code_UA, 1).
is_dcg(code_M, 2).
is_dcg(inline, 1).
is_dcg(mapl, 2).
is_dcg(mapli, 3).
is_dcg(mapllist, 3).
is_dcg(code_fin, 0).
is_dcg(code_binding, 1).
is_dcg(btinit, 3).
is_dcg(fin, 1).
is_dcg(arith_cmp, 3).
is_dcg(std_cmp, 3).
is_dcg(std_eq, 3).
is_dcg(str_eq, 3).
is_dcg(type, 3).
is_dcg(unify_var, 2).
is_dcg(decl, 2).
is_dcg(decl_, 2).
is_dcg(assign, 2).
is_dcg(assign_args, 2).
is_dcg(function, 2).
is_dcg(function_, 1).
is_dcg(off_, 2).
is_dcg(reset_var, 1).
is_dcg(find_fv_, 1).
