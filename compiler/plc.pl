/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

:- initialization(op(1200,xfx,':+')).
:- initialization(op( 900, fy,'+>')).
:- use_module([my_dcg]).
:- use_module(swi).

term_expansion(I,O)	:- translate(I,O).

:- use_module([comp]).

%% These one are only indirectly used
:- use_module([aux, vars, builtin, trad, atoms, map_name, hash]).
:- use_module([modules,trans]).
:- use_module(code).
:- use_module(util).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compile(F)		:- compile(F,[]).

compile(F,Options)	:- process_options(Options),
			   comp_file(F).

%% FIXME 
process_options(_).
