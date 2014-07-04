/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module(foreign, [
		foreign_pred/3,
		foreign_preds/1,
		foreign_preds/3
	]).

:- use_module('pl-ext').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

foreign_pred(PI, C, D) :-
	foreign_pred_builtin(PI, C, D).
foreign_pred(PI, C, D) :-
	recorded(reg_foreign, reg_foreign(PI, C, D)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fpr_info(full, D, PI-C) :-
	foreign_pred(PI, C, D).
fpr_info(spec, D, PI) :-
	foreign_pred(PI, _, D).
fpr_info(functor, D, F) :-
	foreign_pred(F/_, _, D).


foreign_preds(T, D, L) :-
	findall(R, fpr_info(T, D, R), Lr),
	sort(Lr, L).

foreign_preds(L) :-
	foreign_preds(spec, _, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
