/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

:-module(system, [ is_csym/1
		 , shell/1
		 , halt/0
		 , arg/3
		 , gensym/2
		 , format/1
		 , 'C'/3
		 , phrase/3, phrase/2
		 , library_directory/1
		 , once/1 , ignore/1
		 ]).

:- reexport('$bags').
:- reexport('$grammar').
:- reexport('$list').
:- reexport('$apply').

%%
%% End of Interface
%%

%%
is_csym(C)	:- between(0'a,0'z,C).
is_csym(0'_).
is_csym(C)	:- between(0'A,0'Z,C).
is_csym(C)	:- between(0'0,0'9,C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

shell(Cmd)	:- shell(Cmd,0).
halt		:- halt(0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
arg(I,T,V)	:- '$arg'(I,T,V), !.
arg(I,T,V)	:- var(I),
                   functor(T,_,N),
                   between(1,N,I),
                   '$arg'(I,T,V).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gensym(B,V)	:- concat('$gs_', B, K),
		   flag(K, Old, Old+1),
		   succ(Old, New),
		   concat(B, New, V).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
format(F)	:- format(F,[]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

'C'([X|S],X,S).

phrase(Rule, Input, Rest)	:- call(Rule, Input, Rest).
phrase(Rule, Input)		:- phrase(Rule, Input, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% FIXME
library_directory('.').
library_directory('library').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FIXME : meta_predicate ...

ignore(Goal)	:- call(Goal), !.
ignore(_).

once(Goal)	:- call(Goal), !.

