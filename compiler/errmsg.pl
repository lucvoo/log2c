/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module(errmsg, [
		error/1,
		error/2,
		error_report/0,
		fatal/1,
		fatal/2,
		warning/1,
		warning/2
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fmt_msg(Type, Fmt, Args) :-
	concat_atom(['\nPLC: ', Type, ': ', Fmt, '\n'], Msg),
	format(user_error, Msg, Args).
fmt_msg(Type, Arg) :-
	fmt_msg(Type, '~w', [Arg]).

warning(Fmt, Args) :-
	fmt_msg('Warning', Fmt, Args),
	flag(warning, W, W+1).
warning(Arg) :-
	warning('~w', [Arg]).

error(Fmt, Args) :-
	fmt_msg('Error', Fmt, Args),
	flag(error, E, E+1).
error(Arg) :-
	error('~w', [Arg]).

fatal(Fmt, Args) :-
	fmt_msg('Fatal Error', Fmt, Args),
	halt(1).
fatal(Arg) :-
	fatal('~w', [Arg]).


error_report :-
	flag(warning, W, W),
	(   
		W==0
	->
		true
	;
		fmt_msg('Number of warning', W)
	),
	flag(error, E, E),
	(   
		E==0
	->
		fail
	;
		fmt_msg('Number of error', E)
	).
