%% Copyright (c) 1997 Luc Van Oostenryck. All rights reserved.
%%

:- module(vars, [get_lv/4, set_lv/4, vars/3, vars/2]).
%% :- use_module(aux).

%% variables manipulations

get_lv(F,N,V,R):- V=var(_,N),
		   recorded(vars_list,var(F,N),R).
set_lv(F,N,V,R):- erase(R), V=var(_,N),
		  setarg(1,V,F),
		   recorda(vars_list,var(F,N)).

put_lv(I,[V|Qp],Tt,Tv)	:- succ(I,J),
		   V=var(f,J),
		   recorda(vars_list,V),
		   put_lv(J,Qp,Tt,Tv).
put_lv(I,[],[V|Qt],Tv)	:- succ(I,J),
		   V=var(ft,J),
		   recorda(vars_list,V),
		   put_lv(J,[],Qt,Tv).
put_lv(I,[],[],[V|Qv])	:- succ(I,J),
		   V=var(v,J),
		   recorda(vars_list,V),
		   put_lv(J,[],[],Qv).
put_lv(_,[],[],[]).


vars(H,B,R)	:- temp_vars(H,B,Pv,Tt,Tv),
		   length(Pv,R),
		   put_lv(0,Pv,Tt,Tv).

vars(Q,R)	:- % Be sure that no query variable is void or temporary !
		   temp_vars(Q,Pv),
		   length(Pv,R),
		   put_lv(0,Pv,[],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


temp_vars(H,B,Pv,Tt,Tv)	:- to_list(B,Lg), Lg=[G1|Qg],
		   get_void((H,B),Tv),
		   %% maplist(free_variables,[(H,G1)|Qg],LLv),
		   maplist_free_vars([(H,G1)|Qg],LLv),
		   free_variables(LLv,L1),
		   subtract_(L1,Tv,L2),
		   %% sublist(var1(LLv),L2,Tt),
		   sublist_var1(LLv,L2,Tt),
		   subtract_(L2,Tt,Pv).

temp_vars(Q,Pv)	:- to_list(Q,Lg),
		   %% maplist(free_variables,Lg,LLv),
		   maplist_free_vars(Lg,LLv),
		   free_variables(LLv,Pv).

var1([E|Q],V)	:- member_(V,E) -> var2(Q,V)
				;  var1(Q,V).
var2([E|Q],V)	:- member_(V,E) -> fail
			        ;  var2(Q,V).
var2([],_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% void variables

get_void(T,L)	:- get_var(T,Lgv), count_var(Lgv,Lc), keep_void(Lc,L).
get_var(T,L)	:- get_var_(T,Lt), flatten(Lt,L).

get_var_(T,[T])	:- var(T).
%% get_var_(T,L)	:- compound(T), T=..[_|I], maplist(get_var_,I,L).
get_var_(T,L)	:- compound(T), T=..[_|I], maplist_get_var_(I,L).
get_var_(_,[]).

count_var(L,O)	:- count_var(L,[],O).
count_var([],O,O).
count_var([V|Q],I,O)	:- insert_var(V,I,J), count_var(Q,J,O).

insert_var(V,[(W,N)|Qi],J)	:- V==W, succ(N,M), J=[(W,M)|Qi].
insert_var(V,[W|Qi],[W|Qj])	:- insert_var(V,Qi,Qj).
insert_var(V,[],[(V,1)]).

keep_void([(V,1)|Qi],[V|Qo])	:- keep_void(Qi,Qo).
keep_void([_|Qi],Qo)		:- keep_void(Qi,Qo).
keep_void([],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

member_(E,[F|_])	:- E==F.
member_(E,[_|Q])	:- member_(E,Q).

member__([F|_],E)	:- E==F, !.
member__([_|Q],E)	:- member__(Q,E).

member_chk(E,[F|_])	:- E==F, !.
member_chk(E,[_|Q])	:- member_chk(E,Q).

subtract_([],_,[]).
subtract_([E|Q],D,O)	:- ( member_chk(E,D) -> O=Qo; O=[E|Qo] ),
			   subtract_(Q,D,Qo).

list_to_set_([],[])	:- !.
list_to_set_([E|Q],L)	:- ( member_chk(E,Q) -> L=Qo; L=[E|Qo] ),
		           list_to_set_(Q,Qo).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
maplist_free_vars([],[]).
maplist_free_vars([A|X],[B|Y])	:- free_variables(A,B),
				   maplist_free_vars(X,Y).

maplist_get_var_([],[]).
maplist_get_var_([A|X],[B|Y])	:- get_var_(A,B),
				   maplist_get_var_(X,Y).

sublist_var1(_,[],[]).
sublist_var1(G,[A|X],R)   :- var1(G,A)
                        -> R=[A|Y], sublist_var1(G,X,Y)
                        ;  sublist_var1(G,X,R).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
to_list(T,R)		:- to_list(T,R,[]).

to_list((A,B),I,O)	:- to_list(A,I,T), to_list(B,T,O).
to_list((A;B),I,O)	:- to_list(A,I,T), to_list(B,T,O).
to_list((A|B),I,O)	:- to_list(A,I,T), to_list(B,T,O).
to_list((A->B),I,O)	:- to_list(A,I,T), to_list(B,T,O).
to_list((A*->B),I,O)	:- to_list(A,I,T), to_list(B,T,O).
to_list(\+(A),I,O)	:- to_list(A,I,O).
to_list(not(A),I,O)	:- to_list(A,I,O).
to_list(E,[E|O],O).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
