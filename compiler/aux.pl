%% Copyright (c) 1997 Luc Van Oostenryck. All rights reserved.
%%

:- module(aux,  [ read_module/1
		, read_all/2
		, read_export/2
		, file_type/2
		, del_all/0	, del/1
		, new_indent/1
		, warning/1	, warning/2	, fatal/1
		, error/1	, error/2	, error_report/0
		, merge_to_set/3
		, msg_pred_not_def/1	, msg_pred_not_used/1
		, noescape/2
		, to_list/2
		, a_n_f/5
		, fun/4
		, fl/1, fl_/1
		, g/1 , g/2 , g0/1 , g0/2 , f/1 , f/2
		, comm/1 , comm/2 , comm/3 , comm/4 , comm/5
		, comm_pred/2
		, foreign_pred/3	, foreign_preds/1
		, ndet_pred/2	, det_pred/2
		, comp_C/1
		, flag2/3
		, export_pred/1	, exported/1
		, label/2	, label/3 
		, getlabel/2	, getlabel1/3
		, mapl/4
		, mapli/4	, mapli/5
		, module_extension/3
		]).

:- use_module(map_name).
:- use_module('pl-ext').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

foreign_pred(P/I,C,D):-	foreign_pred_builtin(P,I,C,D).
foreign_pred(PI,C,D) :-	recorded(reg_foreign, reg_foreign(PI,C,D)).


ndet_pred(T,L) :-	findall(R, ndet_p(T,R), Lt),
			list_to_set(Lt,L).

ndet_p(F,N,C) :-	foreign_pred(F/N,C,ndet).

ndet_p(full,Np) :-	ndet_p(F,N,C), Np=[F,N,C].
ndet_p(spec,Np) :-	ndet_p(F,N,_), Np=F/N.
ndet_p(functor,Np) :-	ndet_p(F,_,_), Np=F.


det_pred(T,L) :-	findall(R, det_p(T,R), Lt),
			list_to_set(Lt,L).

det_p(F,N,C) :-		foreign_pred(F/N,C,det).

det_p(full,Np) :-	det_p(F,N,C), Np=[F,N,C].
det_p(spec,Np) :-	det_p(F,N,_), Np=F/N.
det_p(functor,Np) :-	det_p(F,_,_), Np=F.


foreign_preds(L) :-	findall(P,preds_C_(P),Lt),
			sort(Lt,L).

preds_C_(F/N) :-	foreign_pred(F/N,_,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
file_type(F,T) :-
	file_base_name(F,Name),
	file_name_extension(Base,Ext,Name),
	( Ext == pl
	  -> true
	  ;  warning('~w : may not be a Prolog file',[F])
	),
	open(F,read,S), set_input(S),
%% 	stream_position(S,P,P),
	stream_property(S,position(P)),
	read(R), 
	( R=':-'(module(M,L))
	  -> ( module_extension(pl,M,Name)
	       -> true
	       ;  warning('file (~w) and module (~w) do not match',[F,M])
	     ),
	     T=module(M,L)
	  ; set_stream_position(S,P),
	    T=user
	),
	flag(input_file,_,Base).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_module(L) :-
	current_input(S),
	readclauses_(S,C,[]),
	flag(current_module,M,M),
	(M==system
	 -> L=C
	 ;  concat('$',_,M)
	    ->	L=C
	    ;	L=[(:- use_module(system))|C]
	),
	set_input(user_input).

readclauses([],O,O)	:- !.
readclauses([F|Q],I,O)	:- readclauses(F,I,T),
			   readclauses(Q,T,O), !.
readclauses(F,I,O)	:- open(F,read,S,[]),
			   readclauses_(S,I,O),
			   close(S).

readclauses_(S,I,O)	:-
	read_term(S,T,[variable_names(V)]),
	(   T==end_of_file
	->  I=O
	;   expand_term(T,Tx),
	    read_Pr(Tx,V,I,Tmp),
	    readclauses_(S,Tmp,O)
	), !.

read_Pr((main :- Q), V, I,O)	:- read_Pr(':-'main(Q,V),V,I,O).
read_Pr(':-'(consult(F)),_, I,O) :- readclauses(F,I,O).
read_Pr(':-'(include(F)),_, I,O) :- readclauses(F,I,O).
read_Pr(':-'(op(P,T,N)),_,[':-'(op(P,T,N))|O],O)	:- op(P,T,N).

read_Pr(T,_,[T|O],O).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
del_all :- del_labels,
	'$erase_records'(code),
	'$erase_records'(vars_list),
	'$erase_records'(directive),
	'$erase_records'(curr_C),
	'$erase_records'(indent),
	'$erase_records'(preds),
	'$erase_records'(export_pred),
	'$erase_records'(used_modules),
	'$erase_records'(module_compiled),
%% DEBUG	'$erase_records'(determinism),
	   flag(indent,_,0), new_indent(0).

del_labels :-	current_flag(K),
		atom(K),
		concat(label_,_,K),
		flag(K,_,0),
		fail.
del_labels.

del(K)		:- '$erase_records'(K).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
new_indent(N)	:- flag(indent,O,O+N).
old_indent(N)	:- flag(indent,O,O-N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
warning(W) :-	format(user_error,'\nWarning: ~w\n',[W]).
warning(W,A) :-	concat_atom([     '\nWarning: ',W,'\n'],W_),
		format(user_error,W_,A).

error(W) :-	format(user_error,'\nError: ~w\n',[W]),
		flag(error,E,E+1).
error(W,A) :-	concat_atom([     '\nError: ',W,'\n'],W_),
		format(user_error,W_,A),
		flag(error,E,E+1).

fatal(W) :-	format(user_error,'\nFatal Error: ~w\n',[W]),
		halt(1).
fatal(W,A) :-	concat_atom([     '\nFatal Error: ',W,'\n'],W_),
		format(user_error,W_,A),
		halt(1).


error_report :-	flag(error,E,E),
		flag(warning,W,W),
		( E==0
		-> fail
		;  format(user_error,'\nNumber of error: ~w\n',[E])
		).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
merge_to_set(L1,L2,S):- list_to_set(L1,S1),
			list_to_set(L2,S2),
			union(S1,S2,S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
msg_pred_not_def(_).
msg_pred_not_used([]).
msg_pred_not_used([P])	:-
	warning('unused predicate : ~w ',[[P]]),
	warning('no code generated for this'),
	format(user_error,'\n',[]).
msg_pred_not_used(Lp)	:-
	warning('unused predicates : ~w ',[Lp]),
	warning('no code generated for these'),
	format(user_error,'\n',[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
noescape(F,Fs)	:-
	atom_codes(F,L),
	noescape_(L,Ls),
	atom_codes(Fs,Ls).
noescape_([],[]).
noescape_([0'\ |Q],[0'\ ,0'\ |R])	:- noescape_(Q,R).
noescape_([0'" |Q],[0'\ ,0'" |R])	:- noescape_(Q,R).
noescape_([E|Q],[E|R])			:- noescape_(Q,R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
to_list(T,R)		:- to_list(T,R,[]).

to_list(A,I,O)		:- var(A), !, I=[A|O].
to_list((A,B),I,O)	:- to_list(A,I,T), to_list(B,T,O).
to_list((A;B),I,O)	:- to_list(A,I,T), to_list(B,T,O).
to_list((A|B),I,O)	:- to_list(A,I,T), to_list(B,T,O).
to_list((A->B),I,O)	:- to_list(A,I,T), to_list(B,T,O).
to_list((A*->B),I,O)	:- to_list(A,I,T), to_list(B,T,O).
to_list(\+(A),I,O)	:- to_list(A,I,O).
to_list(not(A),I,O)	:- to_list(A,I,O).
to_list(E,[E|O],O).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
a_n_f(G,Q,A,F,P):- flag(current_module,M,M),
		   ( M=0 -> true; anf_rec_atom(M) ),
		   anf2([cl([],Q)]),
		   anf1(G), 
		   anf(A,F,P).

anf(A,F,P)	:- flag(current_module,M,M),
		   ( M==system
		     -> foreign_preds(Fps), map(aux:anf_rec_pred,Fps)
		     ;  true
		   ),
		   recorded(used_modules,Ms),
		   map(aux:anf_rec_atom,Ms),
		   anf_rec_import,

		   anf_get_atom(A),
		   anf_get_fun(F),
		   anf_get_pred(P).


anf_rec_import		:- recorded(module_export,module_export(_,F/_)),
			   anf_rec_atom(F),
			   fail.
anf_rec_import.


anf1([])		:- !.
anf1([pr(F,N,L)|Q])	:- anf_rec_pred(F/N),
			   anf2(L), 
			   anf1(Q).

anf2([])		:- !.
anf2([cl(H,G)|Q])	:- to_list(G,L),
			   anf3(L),
			   anf3(H),
			   anf2(Q), !.

anf3([])	:- !.
anf3([E|Q])	:- atom(E), !,
		   anf_rec_atom(E),
		   anf3(Q).
anf3([E|Q])	:- ( var(E); integer(E) ), !,
		   anf3(Q).
anf3([E|Q])	:- anf4(E),
		   anf3(Q).

anf4(E)		:- functor(E,F,N), E=..[F|L],
		   anf_rec_fun(F/N),
		   anf3(L), !.
anf4(_).


anf_rec_atom(A)	:- recorda(anf_rec_atom,A).

anf_rec_fun(F)	:- ( fail %% recorded(anf_rec_fun,F)
                     -> true
                     ;  recorda(anf_rec_fun,F)
                   ),
                   F=A/_ , anf_rec_atom(A).

anf_rec_pred(P)	:- ( fail %% recorded(anf_rec_pred,P)
                     -> true
                     ;  recorda(anf_rec_pred,P)
                   ),
                   anf_rec_fun(P).


anf_get_erase(K,S)	:- '$recorded_all'(K,L), sort(L,S),
                	   '$erase_records'(K).

anf_get_atom(A)	:- anf_get_erase(anf_rec_atom,A).
anf_get_fun(F)	:- anf_get_erase(anf_rec_fun,F).
anf_get_pred(P)	:- anf_get_erase(anf_rec_pred,P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fl_(L)	:- fl(L).
fl(L)	:- format('~w:\n',[L]).
g(F,A)	:- put(9), flag(indent,N,N), tab(N),
	   format(F,A), nl.
g(F)	:- g(F,[]).
g0(F,A)	:- format(F,A), nl.
g0(F)	:- format(F), nl.
f(F,A)	:- tab(9), flag(indent,N,N), tab(N),
	   format(F,A).
f(F)	:- f(F,[]).


comm(H,A,B,C,D)	:- map_name_v(A,Na), map_name_v(B,Nb), map_name_v(C,Nc), map_name_v(D,Nd),
		   format('/* ~w(~w,~w,~w,~w) */\n',[H,Na,Nb,Nc,Nd]), !.
comm(H,A,B,C)	:- map_name_v(A,Na), map_name_v(B,Nb), map_name_v(C,Nc),
		   format('/* ~w(~w,~w,~w) */\n',[H,Na,Nb,Nc]), !.
comm(H,A,B)	:- map_name_v(A,Na), map_name_v(B,Nb),
		   format('/* ~w(~w,~w) */\n',[H,Na,Nb]), !.
comm(H,A)	:- map_name_v(A,Na),
		   format('/* ~w(~w) */\n',[H,Na]), !.
comm(curr_C)	:- flag(curr_C,C,C), getlabel(C,Lab), comm(Lab).
comm(H)		:- format('/* ~w */\n',[H]), !.

comm_pred(F,N)	:- format('/* code for ~w/~w */\n', [F,N]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Read all terms of file 'F' and put it in list 'L'

read_all(F,L)	:- current_input(Old),
		   open(F,read,N),
		   set_input(N),
		   read_all_(L),
		   close(N),
		   set_input(Old).

read_all_(L)	:- read(T),
		   ( T==end_of_file
		     -> L=[]
		     ;  read_all_(Q),
			L=[Tx|Q],
		        expand_term(T,Tx)
		   ).

read_export(F,X):- current_input(Old),
		   open(F,read,N),
		   set_input(N),
		   read_x_(X),
		   close(N),
		   set_input(Old).

read_x_(X)	:- read(T),
		   ( T==end_of_file -> fail;
                     T=export(X)    -> true;
		        read_x_(X)
		   ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
comp_C(F)	:- concat_atom(['make ',F],Make),
		   format(user_error,'~a\n',Make),
		   shell(Make,R), !,
		   ( R = 0
		     -> Res=done
		     ;  Res=failed, flag(error,E,E+1),
		        format(user_error,'\n[ Compilation failed ]\n',[]),
			fail
		   ).
		   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
flag2(K,V1,V2)	:- var(V1), var(V2), !,
		   concat(K,'_1',K1),
		   concat(K,'_2',K2),
		   flag(K1,V1,V1),
		   flag(K2,V2,V2).
flag2(K,V1,V2)	:- concat(K,'_1',K1),
		   concat(K,'_2',K2),
		   flag(K1,_,V1),
		   flag(K2,_,V2).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
export_pred(Xs)	:- format(mod,'export(~q).\n',[Xs]),
		   map_recorda(export_pred,Xs).
map_recorda(_,[]).
map_recorda(K,[A|Q])	:- recorda(K,A),
			   map_recorda(K,Q).

exported(P)	:- recorded(export_pred,P).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fun(T,F,N,A)	:- T=..[F|A], length(A,N).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

label(F,A,Label):- map_fun(F/A,Fm),
		   label(Fm,Label).
label(Fm,Label)	:- concat(Fm,'_',Base),
		   concat(label_,Base,Key),
		   flag(Key, Old, Old+1),
		   succ(Old, New),
		   concat(Base, New, Label).
getlabel(Fm,L)	:- concat(Fm,'_',Base),
		   concat(label_,Base,Key),
		   flag(Key, N, N),
		   concat(Base, N, L).
getlabel1(F,A,L):- map_fun(F/A,Fm),
		   concat(Fm,'_',Base),
		   concat(label_,Base,Key),
		   flag(Key, N, N),
		   succ(N,M),
		   concat(Base, M, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map_name_v(var(_,N),M)	:- concat('_var_',N,M).
map_name_v(atom(E),E).
map_name_v(intg(E),E).
%% map_name_v(float(E),E).
%% map_name_v(string(E),E).
map_name_v(fun(F,_,A),M):- maplist_map_name_v(A,X),
			   M=..[F|X].
map_name_v(E,E)		:- atomic(E).

maplist_map_name_v([],[]).
maplist_map_name_v([A|X],[B|Y])	:- map_name_v(A,B), 
				   maplist_map_name_v(X,Y).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module_transparent	mapl/4,
			mapli/4, mapli/5.

mapl(_,[],L,L).
mapl(G,[E|Q],I,O)	:- call(G,E,I,T),
			   mapl(G,Q,T,O).

mapli(N,G,I,L)		:- mapli(N,G,I,L,[]).

mapli(_,_,[],L,L).
mapli(N,G,[E|Q],I,O)	:- succ(N,M),
			   call(G,M,E,I,T),
			   mapli(M,G,Q,T,O).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

module_extension(X,M,F)	:- concat('$',R,M), !,
			   file_name_extension(R,X,F).
module_extension(X,M,F)	:- M=user, !,
			   flag(input_file,I,I),
			   file_name_extension(I,X,F).
module_extension(X,M,F)	:- file_name_extension(M,X,F).
