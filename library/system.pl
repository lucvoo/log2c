/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module(system, [
		'C'/3,
		arg/3,
		format/1,
		gensym/2,
		halt/0,
		ignore/1,
		is_csym/1,
		library_directory/1,
		once/1,
		phrase/2,
		phrase/3,
		shell/1
	]).

:- reexport('$bags').
:- reexport('$grammar').
:- reexport('$list').
:- reexport('$apply').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
is_csym(C) :-
	between(97, 122, C).
is_csym(95).
is_csym(C) :-
	between(65, 90, C).
is_csym(C) :-
	between(48, 57, C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

shell(Cmd) :-
	shell(Cmd, 0).
halt :-
	halt(0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
arg(I, T, V) :-
	'$arg'(I, T, V), !.
arg(I, T, V) :-
	var(I),
	functor(T, _, N),
	between(1, N, I),
	'$arg'(I, T, V).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gensym(B, V) :-
	concat('$gs_', B, K),
	flag(K, Old, Old+1),
	succ(Old, New),
	concat(B, New, V).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
format(F) :-
	format(F, []).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'C'([X|S], X, S).

phrase(Rule, Input, Rest) :-
	call(Rule, Input, Rest).
phrase(Rule, Input) :-
	phrase(Rule, Input, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% FIXME
library_directory('.').
library_directory(library).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FIXME : meta_predicate ...

ignore(Goal) :-
	call(Goal), !.
ignore(_).

once(Goal) :-
	call(Goal), !.
