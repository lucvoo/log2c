%% Copyright (c) 1997 Luc Van Oostenryck. All rights reserved.
%%

:- module(swi, [ report/1
	       , '$recorded_all'/2
	       , '$erase_records'/1
	       , '$mangle'/2
	       , map/2
	       , hpjw/2
               ]).


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
c_id([C|Q],M)			:- A is C//16, hex_digit(A,XA),
				   B is C mod 16, hex_digit(B,XB),
				   M=[0'_,XA,XB|Mq], c_id(Q,Mq).

hex_digit(V,D)	:- between(0,9,V), D is V + 0'0.
hex_digit(V,D)	:- between(10,15,V), D is V + (0'A-10).


:- meta_predicate map(1, +).

map(G, [E|T])	:- call(G, E), map(G, T).
map(_,[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hpjw(A, H) :-
	atom_codes(A, L),
	hpjw(L, 0, H).

hpjw([], H, H).
hpjw([C|L], S, H) :-
	H1 is S << 4 + C,
	G is H1 /\ 0xf0000000,
	(
		G =\= 0
	->
		H2 is H1 xor (G>>24) xor G
	;
		H2 = H1
	),
	hpjw(L, H2, H).
