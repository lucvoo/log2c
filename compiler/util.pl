%% Copyright (c) 1997 Luc Van Oostenryck. All rights reserved.
%%

:- module(util, [ make_f_args/2, make_ARGs/2
		, find_fvar/2
		, get_preds/2
		, get_query/4
		, anf_module/3
		, init_preds/1
		, decl_preds/1
		, decl_pred/1
		, decl_atoms/1
		, decl_funs/1
		, init_jmp_tbl/2
		, decl_import_mod/1
		, add_module/4
		, meta_pred/2
		, decl_export_mod/1
		, include_module/1
                ]).

:- use_module(aux).
:- use_module([map_name,atoms,modules]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_f_args(N,A)	:- make_f_args_(N,A).
make_f_args_(0,'')	:- !.
make_f_args_(N,A)	:- M is N-1,
			   make_f_args_(M,B), N4 is N+4,
			   concat_atom([B,'FP[',N4,'].celp, '],A).
		   
make_ARGs(0,'')		:- !.
make_ARGs(1,'ARG_1')	:- !.
make_ARGs(N,A)		:- M is N-1,
			   make_ARGs(M,B),
			   concat_atom([B,', ARG_',N],A).
		   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

find_fvar(G,L)	:- to_list(G,Lg),
		   maplist(find_fv,Lg,Lv),
		   flatten(Lv,Lf), sort(Lf,L).
find_fv(G,L)	:- G=..[_|Lg], maplist(find_fv_,Lg,L).

find_fv_(G,L)	:- find_fv_(G,L,[]).
find_fv_(E)	:+ E=var(f,_), +> E.
find_fv_(E)	:+ E=var(ft,_), +> E.
find_fv_(E)	:+ E=fun(_,_,A), maplist(find_fv_,A,L), +> L.
find_fv_(_)	:+ true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_preds(L,Lp)		:- flag(max_tmp,_,0),
			   sortCls_(L,[],Lp), !.

sortCls_([],L,L).
sortCls_([C|Q],I,O)	:- getC(C,Ci),
			   max_tmp(C),
		           sortCls_(Q,I,IO),
		           insertC(Ci,IO,O).


getC(H :- B,cl(F,N,A,B ))	:- fun(H,F,N,A).
getC( ':-'(main(Q,V)),q(V,Q)).
getC(  :- D,'')			:- fun(D,F,N,_),
                                   ( directive(F,N)
                                     -> do_directive(D)
                                     ;  warning('unknow directive : ~w/~w',[F,N])
                                        %% , call(D)
                                   ).
getC(H,     cl(F,N,A,true))	:- fun(H,F,N,A).

insertC('',L,L).
insertC(q(V,Q),I,O) :- select(I,q(Lq),Is), !, O=[q([cl(V,Q)|Lq]) |Is].
insertC(q(V,Q),I,O) :- O=[q([cl(V,Q)]) |I].
insertC(cl(F,N,A,B),I,O) :- select(I,pr(F,N,Lc),Is), !, O=[pr(F,N,[cl(A,B)|Lc]) |Is].
insertC(cl(F,N,A,B),I,O) :- O=[pr(F,N,[cl(A,B)]) |I].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_query(I,Q,B,O)	:- select(I,q(Lq),O), check_query(Lq,Q,B), !.
get_query(_,_,_,_)	:- fatal('no query given').

check_query([cl(B,Q)],Q,B).
check_query(_ ,_,_)	:- fatal('several queries given').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

anf_module(La,Lf,Lp)	:- flag(current_module,M,M),
			   module_extension(h,M,H),
			   format('#include <Prolog.h>\n#include <pl-trad.h>\n\n',[]),
			   format('#include "~w"~n~n',[H]),
			   map_atom(M,Mm),
			   format(h,'#ifndef MODULE~w_H_~n#define MODULE~w_H_~n~n',[Mm,Mm]),
			   used_modules(Ms),
			   map(include_module,Ms),
			   nl,
			   flag(max_arg,_,0),
			   decl_atoms(La),
			   decl_funs(Lf),
			   init_preds(Lp),
			   init_args.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_args		:- nl,
			   flag(max_arg,A,A),
			   between(1,A,I),
			   format('cell_t *ARG_~w;\n',[I]), fail; true,
			   nl,
			   flag(max_tmp,T,T),
			   between(1,T,I),
			   format('cell_t *TMP_~w;\n',[I]), fail; true,
			   nl.
init_preds([F|Q])	:- recorda(preds,F),
			   map_pred(F,Pm),
	                   format(h,'extern void PRED~w;\n',Pm),
			   init_preds(Q).
init_preds([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
decl_export_mod(export(X,L))	:- flag(current_module,M,M),
				   map(decl_exp_mod(M,X),L).

decl_exp_mod(M,X,P)	:- map_pred(P,M,Pm),
			   map_pred(P,X,Px),
			   format(h,'#define PRED~w PRED~w~n',[Pm,Px]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decl_preds(X)	:- '$recorded_all'(export_pred,P),
		   append(X,P,T), sort(T,L),
		   map(decl_pred,L),
		   nl.

decl_pred(P)	:- import_from_module(P,M),
		   map_pred(P,M,Pm),
		   format(h,'extern void PRED~w;\n',Pm).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decl_atoms(As)	:- map(decl_atoms_,As),
		   nl(h).
decl_atoms_(A)	:- map_atom(A,Am),
		   format(h,'extern atom__t ATOM_~w;\n',[Am]),
		   format(mod,'~q.\n',[atoms(A)]).

decl_funs(Fs)	:- map(decl_funs_,Fs),
		   nl(h).

decl_funs_(F/N)	:- map_atom(F,Fm),
		   format(h,'extern fun__t FUN_~w_~d;\n',[Fm,N]),
		   format(mod,'~q.\n',[funs(F/N)]),
		   flag(max_arg,O,max(O,N)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_jmp_tbl(import,M)	:- map_atom(M,Mm),
			   format('&module~w, ',Mm).

init_jmp_tbl(pub,FN)	:- \+ exported(FN), !.
init_jmp_tbl(_,F/N)	:- map_atom(F,Fm),
			   format('{ FUN(~w,~d), &&~w_~d_1}, ',[Fm,N,Fm,N]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decl_import_mod(M)	:- map_atom(M,Mm),
			   format('extern module_t module~w;\n',Mm).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

max_tmp(T)	:- free_variables(T,L),
		   length(L,N),
		   flag(max_tmp,O,max(O,N)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_module(M,I,A,Arg)	:- add_module(1,I,M,A,Arg).

add_module(_,_,_,[],[]).
add_module(I,I,M,[A|X],[fun((:),2,[atom(M),A])|X])	:- !.
add_module(I,N,M,[A|X],[A|Y])	:- succ(I,J),
				   add_module(J,N,M,X,Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

directive(use_module,1).
directive(export_module,1).
directive(index,1).
directive(meta_pred,2).
directive(meta,1).
directive(module_transparent,1).
directive(op,3).


do_directive(module_transparent(P))	:- do_directive(meta(P)).
do_directive(meta_pred(P,A))		:- recordz(meta_pred,meta_pred(P,A)),
					   recordz(meta,P).
do_directive(meta(P))		:- recordz(meta,P).
do_directive(meta((P,L)))	:- recordz(meta,P), do_directive(meta(L)).
do_directive(op(P,T,N))		:- op(P,T,N).
do_directive(use_module(M))	:- recorda(use_module,M).
do_directive(export_module(M))	:- recorda(export_module,M), recorda(use_module,M).

do_directive(D)			:- recordz(directive,D).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
meta_pred(P,I)		:- recorded(meta_pred,meta_pred(P,I)).
meta_pred(maplist/3,1).
meta_pred(findall/3,2).
meta_pred(bagof/3,2).
meta_pred(setof/3,2).
meta_pred(map/2,1).
meta_pred(mapi/3,2).
meta_pred(mapl/3,1).
meta_pred(mapl/4,1).
meta_pred(mapli/4,2).
meta_pred(mapli/5,2).
meta_pred(call/1,1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
include_module(M)	:- module_extension(h,M,H),
			   format('#include "~w"\n',[H]).

