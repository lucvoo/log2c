%% Copyright (c) 1997 Luc Van Oostenryck. All rights reserved.
%%

:- module(atoms, [ map_atom/2
                 , atoms/1
                 , functors/1
                 %% , 'C_id'/2
                 ]).

:- index(map_atom(1,1)).


map_atom(A,N)	:- '$mangle'(A,N).


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

		%% pl-time.c
		, dt
		, cpu_time, user_time, system_time, real_time, runtime
		, local_stack, heap_stack, sheap_stack, trail_stack

		%% pl-file.c
		, '$stream_position', ''
		, none, execute, exist

		%% diverse
		, '.' , [], '$VAR'

		%% pl-write.c
		, quoted
		, char_escape
		, ignore_ops
		, numbervars
		, namevars
		, bindvars
		, max_depth
		, space_args
		, list_notation
		, curly_notation
	       ],
	   A = L.

functors(F)	:- L =	[ (*)/2 , (+)/2 , (-)/1 , (-)/2 , (.)/2 , (,)/2
			, (/)/2 , (//)/2 , (:)/2 , '{}'/1 , pipe/1
			, (=)/2
			, '$stream_position'/3
			, (max)/2, (min)/2, (mod)/2
			, '$VAR'/1, '$VARNAME'/1

		%% pl-io.c
			, file_name/1, mode/1, type/1, alias/1
			, position/1, end_of_stream/1
			, eof_action/1, reposition/1

		%% pl-network.c
			, ip/4

		%% pl-time.c
			, dt/6

                       ],
                   F = L.

