%% Copyright (c) 1997 Luc Van Oostenryck. All rights reserved.
%%

:- module(atoms, [ map_atom/2
                 , atoms/1
                 , functors/1
                 %% , 'C_id'/2
                 ]).

:- index(map_atom(1,1)).

%% map_atom((/\),	and).
%% map_atom((=:=),	ar_equals).
%% map_atom((>),	ar_larger).
%% map_atom((>=),	ar_larger_equal).
%% map_atom((=\=),	ar_not_equal).
%% map_atom((<),	ar_smaller).
%% map_atom((=<),	ar_smaller_equal).
%% map_atom((=@=),	at_equals).
%% map_atom((@>),	at_larger).
%% map_atom((@>=),	at_larger_equal).
%% map_atom((\=@=),at_not_equals).
%% map_atom((@<),	at_smaller).
%% map_atom((@=<),	at_smaller_equal).
%% map_atom((|),	bar).
%% map_atom((,),	comma).
%% map_atom(({}),	curl).
%% map_atom((//),	div).
%% map_atom((/),	divide).
%% map_atom((.),	dot).
%% map_atom((**),	doublestar).
%% map_atom((-->),	grammar).
%% map_atom((^),	hat).
%% map_atom((->),	ifthen).
%% map_atom((<<),	lshift).
%% map_atom((-),	minus).
%% map_atom((:),	module).
%% map_atom(([]),	nil).
%% map_atom((\+),	not_provable).
%% map_atom((\==),	not_strick_equals).
%% map_atom((\=),	not_unifiable).
%% map_atom((?),	obtain).
%% map_atom((\/),	or).
%% map_atom((+),	plus).
%% map_atom((:-),	prove).
%% map_atom((?-),	query).
%% map_atom((>>),	rshift).
%% map_atom((;),	semicolon).
%% map_atom((*->),	softcut).
%% map_atom((*),	star).
%% map_atom((==),	strick_equals).
%% map_atom((=),	unify).
%% map_atom((=..),	univ).
%% map_atom((\),	backslash).
%% map_atom('$stream_position',	str_pos).

map_atom(A,N)	:- '$mangle'(A,N).

%% map_atom(A,N)	:- !, atom(A), 'C_id'(A,N).
%% 
%% 'C_id'(A,Ma)	:- atom_chars(A,L), c_id(L,Ml), !, atom_chars(Ma,[0'_|Ml]).
%% 
%% c_id([],[])	:- !.
%% c_id([0'_|Q],[0'_,0'_|Mq])	:- !, c_id(Q,Mq).
%% c_id([C|Q],[C|Mq])	:- is_csym(C), !, c_id(Q,Mq).
%% c_id([C|Q],M)		:- _A is C//16, hex_digit(_A,A),
%% 			   _B is C mod 16, hex_digit(_B,B),
%% 			   M=[0'_,A,B|Mq], c_id(Q,Mq).
%% 
%% hex_digit(V,D)	:- between(0,9,V), D is V + 0'0.
%% hex_digit(V,D)	:- between(10,15,V), D is V + (0'A-10).


atoms(A):- L = [ []
		%% pl-op.c
		, (:-) , ('|'), (*->), (,), (-->), (->), (;)
		, (:+), (+>)
		, * , ** , + , - , / , // , /\ , \/ , << , >> , \ , ^
		, is , mod , rem , xor
		, = , \= , == , \== , =@= , \=@= , =:= , =\=
		, < , > , =< , >= , @< , @=< , @> , @>=
		, =..  , : , {} , \+ , not
		, fx , fy , xf , yf , xfx , xfy , yfx , yfy
		, (module_transparent)
		% , (discontiguous) , (dynamic) , (initialization) , (multifile) , (volatile)

		%% pl-io.c
		, user , user_input , user_output , user_error , stderr
		, type , alias , eof_action , eof_code , reposition
		, text , binary , error , reset , end_of_file
		, read , write , append, update
		, input, output, at, past, not

		%% pl-option.c
		, true , false , on , off

		%% pl-rec.c
		, mark

		%% pl-read.c
		, syntax_errors, variable_names, singletons
		, term_position, subterm_positions
		, fail, quiet, end_of_file

		%% pl-file.c
		, '$stream_position', ''
		, none, execute, exist

		%% diverse
		, '.' , [], '$VAR'
	       ],
	   A = L.

functors(F)	:- L =	[ (*)/2 , (+)/2 , (-)/1 , (-)/2 , (.)/2 , (,)/2
			, (/)/2 , (//)/2 , (:)/2 , '{}'/1 , pipe/1
			, (=)/2
			, '$stream_position'/3
			, (max)/2, (min)/2, (mod)/2
			, '$VAR'/1
		%% pl-io.c
			, file_name/1, mode/1, type/1, alias/1
			, position/1, end_of_stream/1
			, eof_action/1, reposition/1
                       ],
                   F = L.

