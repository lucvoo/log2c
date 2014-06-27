/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module(foreign, [
		foreign_pred/3,
		foreign_preds/1
	]).

:- use_module('pl-ext').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

foreign_pred(P/I, C, D) :-
	foreign_pred_builtin(P, I, C, D).
foreign_pred(PI, C, D) :-
	recorded(reg_foreign, reg_foreign(PI, C, D)).


foreign_preds(L) :-
	findall(PI, foreign_pred(PI,_,_), Lt),
	sort(Lt, L).
