/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

:- module('$apply', [ maplist/3
		    , checklist/2 , map/2
		    , sublist/3
		    %% , forall/2
		    ]).

%% FIXME : meta_predicate ...

maplist(G,[A|X],[B|Y])	:- call(G,A,B), maplist(G,X,Y).
maplist(_,[],[]).


checklist(G,[A|X])	:- call(G,A), checklist(G,X).
checklist(_,[]).

%% same as checklist/2 (shorter name).
map(G,[A|X])	:- call(G,A), map(G,X).
map(_,[]).


sublist(G,[A|X],R)	:- call(G,A,B), !,
			   R=[B|Y],
			   sublist(G,X,Y).
sublist(G,[_|X],R)	:- sublist(G,X,R).
sublist(_,[],[]).

%% forall(Cond, Action) :-
%%         \+ (Cond, \+ Action).
