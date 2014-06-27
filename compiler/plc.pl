/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- op(1200, xfx, :+).
:- op(900, fy, +>).
:- use_module([my_dcg]).
:- use_module(swi).

term_expansion(I, O) :-
	translate(I, O).

:- use_module(comp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compile(F) :-
	compile(F, []).

compile(F, Opt) :-
	process_options(Opt),
	comp_file(F).

process_options(_).
