%% Copyright (c) 1997 Luc Van Oostenryck. All rights reserved.
%%

:- module(swi, [ report/1
	       , '$recorded_all'/2
	       , '$erase_records'/1
	       , '$mangle'/2
	       , map/2
%	       , atom_codes/2
               ]).

%% ISO compability
%% atom_codes(A,C)	:- atom_chars(A,C).

report(T)	:- write(user_error,T),
		   put(user_error,'\n').


'$recorded_all'(K,L)	:- findall(R,recorded(K,R),L).

'$erase_records'(K)	:- recorded(K,_,R),
			   erase(R),
			   fail.
'$erase_records'(_).



'$mangle'(A,Ma)	:- atom_codes(A,L),
		c_id(L,Ml), !,
		atom_codes(Ma,[0'_|Ml]).

c_id([],[])			:- !.
c_id([0'_|Q],[0'_,0'_|Mq])	:- !, c_id(Q,Mq).
c_id([C|Q],[C|Mq])		:- is_csym(C), !, c_id(Q,Mq).
c_id([C|Q],M)			:- _A is C//16, hex_digit(_A,A),
				   _B is C mod 16, hex_digit(_B,B),
				   M=[0'_,A,B|Mq], c_id(Q,Mq).

hex_digit(V,D)	:- between(0,9,V), D is V + 0'0.
hex_digit(V,D)	:- between(10,15,V), D is V + (0'A-10).


:- module_transparent map/2.

map(G, [E|T])	:- call(G, E), map(G, T).
map(_,[]).


