%% Copyright (c) 1997 Luc Van Oostenryck. All rights reserved.
%%

:- module(modules, [ check_export/2
		   , check_export/3
		   , check_import/2
		   , get_exports/2
		   , export_user_preds/1
		   , read_mods/4
		   , used_modules/1
		   , need_modules/1
		   , module_path/2
		   , import_from_module/2
		   , check_module/1
		   ]).

:- use_module(aux).
:- use_module('pl-ext').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_export(X1,L)	:- flag(current_module,M,M),
			   map(check_export1(M,L),X1),
			   ( M==system
			     -> foreign_preds(X2), append(X1,X2,Xs)
			     ;  Xs=X1
			   ),
			   export_pred(Xs).

check_export(X1,L,Xm)	:- flag(current_module,M,M),
			   map(check_export1(M,L),X1),
			   ( M==system
			     -> foreign_preds(X2), append(X1,X2,X3)
			     ;  X3=X1
			   ),
			   export_l(Xm,Xl,X3),
			   sort(Xl,Xs),
			   export_pred(Xs).

check_export1(M,L,F/N)	:- ( memberchk(pr(F,N,_),L)
			     -> true
		             ;  warning('Exported predicate: ~w:~w/~w is not defined',[M,F,N])
			   ).

export_l([],L,L).
export_l([M|Q],I,O)		:- export_l_(M,I,N),
				   export_l(Q,N,O).
export_l_(export(_,L),I,O)	:- append(L,O,I).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export_user_preds(pr(F,N,_))
		:- recorda(export_pred,F/N),
		   recorda(module_export,module_export(user,F/N)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_mods(M,A,F,P)	:- del(need_module),
			   read_mod(M,A,[],F,[],P,[]).

read_mod([],A,A,F,F,P,P).
read_mod([M|Q],Ia,Oa,If,Of,Ip,Op)	:-
		   read_mod(M,Ia,Ta,If,Tf,Ip,Tp),
		   read_mod(Q,Ta,Oa,Tf,Of,Tp,Op).
read_mod(M,Ia,Oa,If,Of,Ip,Op)	:-
		   ( recorded(need_module,M) 
		     -> Ia=Oa, If=Of, Ip=Op
		     ; recorda(need_module,M),
		       comp_sub_module(M,F),
		       read_all(F,L),
		       read_mod_(L,Ia,Oa,If,Of,Ip,Op)
		   ).

read_mod_([],A,A,F,F,P,P).
read_mod_([T|Q],Ia,Oa,If,Of,Ip,Op)  :-
		read_mod__(T,Ia,Ta,If,Tf,Ip,Tp),
		read_mod_(Q,Ta,Oa,Tf,Of,Tp,Op).

read_mod__(atoms(A),Ia,Oa,F,F,P,P)	:- Ia=[A|Oa], !.
read_mod__(funs(F),A,A,If,Of,P,P)	:- If=[F|Of], !.
read_mod__(export(P),A,A,F,F,Ip,Op)	:- append(P,Op,Ip), !.
read_mod__(use_module(M),Ia,Oa,If,Of,Ip,Op) :- read_mod(M,Ia,Oa,If,Of,Ip,Op), !.
read_mod__(T,A,A,F,F,P,P)                   :- recorda(module_info,T), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
comp_sub_module(M,P)	:- recorded(module_compiled,M),
			   module_path(M,P).
comp_sub_module(M,F)	:- flag(input_file,I,I),
			   M=I,
			   module_path(M,F).
comp_sub_module(M,F)	:- module_path(M,F),
			   format(user_error,'[ Compiling module ~w :',M),
%% stable		   concat_atom(['make ',F,' > /dev/null 2>&1'], Make),
			   concat_atom(['make ',F], Make),
		           shell(Make,R),
			   ( R = 0
			     -> Res=done
			     ;  Res=failed, flag(error,E,E+1)
			   ),
			   format(user_error,' ~w ]\n',[Res]),
			   recorda(module_compiled,M).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
module_path(M,F)	:- module_extension('.mod',M,F).
module_source(M,F)	:- module_extension('.pl',M,F).  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
used_modules(L)		:- export_use_(use_module,used_modules,L).
exported_modules(L)	:- export_use_(export_module,exported_modules,L).

export_use_(_,S,L)	:- recorded(S,L), !.
export_use_(T,S,L)	:- '$recorded_all'(T,Lm),
			   flatten(Lm,B), sort(B,L),
			   recorda(S,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_exports(Lu,Lx)	:- used_modules(U),
			   exported_modules(X),
			   maplist(get_xlist,U,Xs),
			   maplist(prefix_export(Xs,use),U,Lu),
			   maplist(prefix_export(Xs,export),X,Lx).

get_xlist(M,A)	:- comp_sub_module(M,F), !,
		   read_export(F,X),
		   A=(M-X).

prefix_export(L,P,M,R)	:- memberchk(M-Xs,L),
			   R=..[P,M,Xs].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
need_modules(Ms)	:- '$recorded_all'(need_module,Ms).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_import(U,_X)	:- del(module_export),
			   used_modules(Us),
			   format(mod,'use_module(~q).\n',[Us]),
			   map(rec_x,U).
                         
rec_x(use(M,L))	:- map(rec_export(M),L).

rec_export(M,X)	:- ( recorded(module_export,module_export(Mm,X))
		     -> X=F/N,
			error('Predicate ~w/~w already imported from module ~w',[F,N,Mm])
		     ;  recorda(module_export,module_export(M,X))
		   ).

exp_x(export(_,L))	:- map(export_pred,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
import_from_module(F,M)	:- recorded(module_export,module_export(M,F)).
import_from_module(_,M)	:- flag(current_module,M,M).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_module(P)		:- %% predicate imported from a module
			   recorded(module_export,module_export(_,P)), !.
check_module(P)		:- %% private predicate
			   recorded(preds,P), !.
check_module(F/N)	:- foreign_pred(F/N,_,ndet).
check_module(F/N)	:- ( recorded(undef_pred,undef_pred(F,N))
			     -> true
			     ;  recorda(undef_pred,undef_pred(F,N)),
			        error('Undefined predicate: ~w/~w',[F,N])
			   ).
			   %% further compilation will fail but
			   %% we want message for other possible errors
