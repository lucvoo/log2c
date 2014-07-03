/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module(input, [
		file_type/3,
		read_module/2
	]).

:- use_module(errmsg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%!	file_type(+File, -Type, -Stream) is semidet
%
%	Open File, return the corresponding Stream
%	and return the 'type' of the file:
%	- 'module(M, X)' is the file begins with a module directive
%	- 'user' otherwise
file_type(F, T, S) :-
	file_base_name(F, Name),
	file_name_extension(Base, Ext, Name),
	(   
		Ext==pl
	->
		true
	;
		warning('~w : may not be a Prolog file', [F])
	),
	open(F, read, S),
	stream_property(S, position(P)),
	read(S, R),
	(   
		R= (:-module(M, L))
	->
		%% (   
		%% 	module_filename(pl, M, Name)
		%% ->
		%% 	true
		%% ;
		%% 	warning('file (~w) and module (~w) do not match', [F, M])
		%% ),
		T=module(M, L)
	;
		set_stream_position(S, P),
		T=user
	),
	flag(input_file, _, Base).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_module(S, L) :-
	readclauses_(S, C, []),
	flag(current_module, M, M),
	(   
		M==system
	->
		L=C
	;
		
		concat($, _, M)
	->
		L=C
	;
		L=[ (:-use_module(system))|C]
	).

readclauses([], O, O) :- !.
readclauses([F|Q], I, O) :-
	readclauses(F, I, T),
	readclauses(Q, T, O), !.
readclauses(F, I, O) :-
	open(F, read, S, []),
	readclauses_(S, I, O),
	close(S).

readclauses_(S, I, O) :-
	read_term(S, T, [variable_names(V)]),
	(   
		T==end_of_file
	->
		I=O
	;
		expand_term(T, Tx),
		read_Pr(Tx, V, I, Tmp),
		readclauses_(S, Tmp, O)
	), !.

read_Pr((main:-Q), V, I, O) :-
	read_Pr((:-main(Q, V)), V, I, O).
read_Pr((:-consult(F)), _, I, O) :-
	readclauses(F, I, O).
read_Pr((:-include(F)), _, I, O) :-
	readclauses(F, I, O).
read_Pr((:-op(P, T, N)), _, [ (:-op(P, T, N))|O], O) :-
	op(P, T, N).

read_Pr(T, _, [T|O], O).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
