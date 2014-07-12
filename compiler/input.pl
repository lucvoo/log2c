/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module(input, [
		file_type/4,
		read_module/2
	]).

:- use_module(errmsg).
:- use_module(export).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%!	file_type(+File, -Type, -Stream) is semidet
%
%	Open File, return the corresponding Stream
%	and return the 'type' of the file:
%	- 'module(M, X)' is the file begins with a module directive
%	- 'user' otherwise
file_type(F, Fname, M, S) :-
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
		R= (:-module(M, Xs))
	->
		%% (   
		%% 	module_filename(pl, M, Name)
		%% ->
		%% 	true
		%% ;
		%% 	warning('file (~w) and module (~w) do not match', [F, M])
		%% ),
		export_add(Xs),
		Fname = M
	;
		set_stream_position(S, P),
		Fname = Base,
		M = user,
		true
	),
	flag(input_file, _, Base).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- meta_predicate mapelems(1, ?).
mapelems(G, [A|X]) :-
	call(G, A),
	mapelems(G, X).
mapelems(_, []).
mapelems(G, (A,X)) :-
	call(G, A),
	mapelems(G, X).
mapelems(G, A) :-
	call(G,A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_module(S, L) :-
	read_items(S, L, []), !.

read_files([], O, O).
read_files([F|Q], I, O) :-
	read_file(F, I, T), !,
	read_files(Q, T, O).
read_files(F, I, O) :-
	read_file(F, I, O).
read_file(F, I, O) :-
	open(F, read, S, []),
	read_items(S, I, O),
	close(S).

read_items(S, I, O) :-
	read_term(S, T, [variable_names(V)]),
	(   
		T==end_of_file
	->
		I=O
	;
		expand_term(T, Tx),
		read_item(Tx, V, I, Tmp),
		read_items(S, Tmp, O)
	).

read_item((main:-Q), V, I, O) :-
	read_item((:-main(Q, V)), V, I, O).
read_item((:-consult(F)), _, I, O) :-
	read_files(F, I, O).
read_item((:-include(F)), _, I, O) :-
	read_files(F, I, O).
read_item((:-Op), _, [Op|O], O) :-
	Op = op(_, _, _),
	recordz(export_ops, Op),
	call(Op).

read_item(T, _, [T|O], O)	:-
	process_item(T).


%%%%
:- use_module(modules).
process_item((:- use_module(M)))	:-
	!,
	load_mod(M).
process_item(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
