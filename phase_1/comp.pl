%% Copyright (c) 1997 Luc Van Oostenryck. All rights reserved.
%%

%% :- use_module([my_dcg]).
%% :- use_module(swi).
%% term_expansion(I,O)	:- translate(I,O).
:- module(comp, [ comp_file/1 ] ).

:- use_module([aux, vars, builtin, trad, atoms, map_name, hash]).
:- use_module([modules,trans]).
:- use_module(code).
:- use_module(util).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_all		:- init_hash,
			   del_all.

comp_file(File)		:- comp_file(File,[]).
comp_file(File,Opt)	:- init_all,
			   file_type(File,Type),
			   ( Type=user -> comp_user(Opt) ;
			     Type=module(M,X) -> comp_module(M,X,Opt)
			                      ;  fail
			   ).

comp_module(Mod,Export,_Opt) :-
		   flag(current_module,_,Mod),
		   open_files(Mod,_Fc,Fm,_Fh),
		   set_output(c),
		   read_module(Li), 
		   code_module(Li,Export,Lo), !,
		   trad(Lo), nl,
		   init_hash_jmps,
		   set_output(user_output),
		   close(c), close_h, close(mod),
		   ( error_report
		     -> delete_file(Fm), % delete_file(_Fc), delete_file(_Fh),
		        halt(1)
		     ;  module_extension(o,Mod,_Fo),
			( true %% comp_C(_Fo)	%% FIXME :
			  -> true % delete_file(_Fc)%% leave the file for
			  ;  true		%% inspection in case
			)			%% of error
		   ).


comp_user(Opt)	:- flag(current_module,_,user),
		   read_module(Li), 
		   flag(input_file,Name,Name),
		   open_files(Name,_Fc,_Fm,_Fh),
		   set_output(c),
		   code_user(Li,Opt), !,
		   init_hash_jmps, 
		   close(mod),
		   nl,
		   set_output(user_output),
		   close(c), !,
		   ( error_report
		     -> true
		     ;  link_file(Name)
		   ).
		   % delete_file(File_c),
		   % delete_file(File_mod).
		   
open_files(Name,C,H,M)	:- module_extension(c,  Name, C),
			   module_extension(mod,Name, M),
			   module_extension(h,  Name, H),
			   open(C,   write,c),
			   open(M,write,mod),
			   open(H,write,h).

close_h	:- format(h,'~n#endif~n',[]),
	   close(h).
link_file(Name)	:- flag(input_file,_,Name),
		   module_extension('lnk.c', Name,File_Lnk),
		   open(File_Lnk,write,lnk),
		   set_output(lnk),
		   format('#include <Prolog.h>\n#include <pl-trad.h>\n\n'),
		   code_anf(Name),
		   init_hash_mods(Name),
		   set_output(user_output),
		   close_h, close(lnk), !,
		   ( error_report
		     -> fail
		     ;  ( link(Name)
		          -> true % delete_file(File_Lnk)
		          ;  fail
			)
		   ).

code_anf(N)	:- read_mods(N,A,F,P),
		   findall(V,recorded(module_export,module_export(user,V)),Puser),
		   append(P,Puser,Pall),
		   init_hash(A,F,Pall).

link(Name)	:- need_modules(Ms),
		   maplist(module_extension(o),Ms,Mso_),
		   sort(Mso_,Mso),
		   concat_atom(Ms,' ',L),
		   concat_atom(Mso,' ',Lo),
		   concat_atom(['make PROG="',Name,'" MODULES="',Lo,'" ',Name], Make),
		   format(user_error,'~w\n',[Make]),
		   format(user_error,'[ Linking module(s) ~w :',[L]),
		   shell(Make,R), !,
		   ( R = 0
		     -> format(user_error,' done ]\n',[])
		     ;  format(user_error,' failed ]\n',[]),
		        flag(error,E,E+1),
			fail
		   ).

code_user(I,Opt)	:- code_user(I,Opt,T,[]), trad(T).
code_user(I,_Opt)	:+ get_preds(I,Lpr),
		           get_query(Lpr,Q,B,P),
			   get_exports(Us,Xs),
			   check_import(Us,Xs),
			   flag(current_module,M,M),
			   map(export_user_preds,P),	%% export all predicates
			   init_module(P,Q,Xs),
			   code_Q(Q),
			   code_P(P),
			   code_fin,
			   code_binding(B).

code_module(I,X,O)	:- code_module(I,X,O,[]).
code_module(I,X)	:+ get_preds(I,P),
			   get_exports(Us,Xs),
			   check_import(Us,Xs),
			   check_export(X,P,Xs),	%% check and export public predicates
			   init_module(P,[],Xs),
			   code_P(P),
			   flag(current_module,M,M),
			   ( M==system -> code_FPr; true ),
			   code_fin.

init_module(P,Q,X)	:- del(undef_pred),
			   a_n_f(P,Q,La,Lf,Lp),
			   anf_module(La,Lf,Lp),
			   flag(current_module,M,M),
			   map_atom(M,Mod),
			   used_modules(Ms),
			   map(decl_import_mod,Ms),
			   map(decl_export_mod,X), nl,
			   format('\nvoid module_~w(void)\n{\n',[Mod]),
		           format('  if (&&backtrack==0) return;\n\n').

get_atom_from_fun(A/_,A).

init_hash(La,Lf,Lp)	:- atoms(Ba),
			   functors(Bf),
			   append(La,Ba,Ca),
			   append(Lf,Bf,F),
			   maplist(get_atom_from_fun,F,Af),
			   append(Ca,Af,A),
			   sort(A,Sa), sort(F,Sf), sort(Lp,Sp),
			   init_hash_atoms(Sa),
			   decl_preds(Sp),
			   init_hash_funs(Sf).


code_fin	:+ +> backtrack,
		   +> format('}\n').

code_jump_table(Lp) :- 
		   format('  static jmp_t all_preds[] = { '),
		   map(init_jmp_tbl(all),Lp),
	           format('{0, 0} };\n'),
		   format('  static jmp_t pub_preds[] = { '),
		   map(init_jmp_tbl(pub),Lp),
	           format('{0, 0} };\n'),
		   format('  static module_t import_mod[] = { '),
		   used_modules(Ms),
		   map(init_jmp_tbl(import),Ms),
	           format('0 };\n'),
		   flag(current_module,M,M),
		   map_atom(M,Mod),
		   format('  static module__t module~w asm("module~w") = { ATOM(~w), all_preds, pub_preds, import_mod };\n\n',[Mod,Mod,Mod]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
code_P(P,L,L)	:- code_P(P).
code_P([P|Q])	:- code_Pr(P,T,[]), trad(T), code_P(Q).
code_P([]).

code_Pr(pr(F,A,[C]))	:+ comm('code for ~w/~w',F,A),
			   flag2(curr_P,F,A),
			   ( recorded(meta,F/A)
			     -> flag(meta,_,true)
			     ;  flag(meta,_,false)
			   ),
			   code_C(F,A,C,single).
code_Pr(pr(F,A,[C|Q]))	:+ comm('code for ~w/~w',F,A),
			   flag2(curr_P,F,A),
			   ( recorded(meta,F/A)
			     -> flag(meta,_,true)
			     ;  flag(meta,_,false)
			   ),
			   code_C(F,A,C,first),
			   code__Pr(pr(F,A,Q)).

code__Pr(pr(F,A,[C]))	:+ code_C(F,A,C,last).
code__Pr(pr(F,A,[C|Q]))	:+ code_C(F,A,C,middle),
			   code__Pr(pr(F,A,Q)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
code_FPr	:+ ndet_pred(full,Ln),
		   mapl(code_FPr_ndet,Ln),
		   det_pred(full,Ld),
		   mapl(code_FPr_det,Ld).

code_FPr_ndet([F,N,C]) :+
		+> comm('code for ~w/~w',F,N),
		L is N + 4, L1 is L+1,
		+> flag(arg,_,fp4), flag(arg,_,fp4),
		flag(rho,_,L),
		label(F,N,Li), flag(curr_C,_,Li), label(Li,_),
		+> fl(Li),
		map_pred(F/N,Pm), map_fun(F/N,Fm),
		( exported(F/N)
	          -> +> g0('asm(".global PRED~w");',[Pm])
	          ;  true
	        ),
	        +> g0('asm("PRED~w:" : : "p" (&&~w_1));',[Pm,Fm]),
		getlabel1(F,N,Lo),
		btinit(first,Lo,N),
		+> pushenv(L1),
		+> g('FP[~w].ctrl=FIRST_CALL;',[L1]),
		+> fl(Lo),
		make_f_args(N,Args),
		+> g('switch (~w(~w&(FP[~w].ctrl)))',[C,Args,L1]),
		+> g('{ case SUCCEED: delbtp();'),
		+> g('                popenv();'),
		+> g('                break;'),
		+> g('  case FAIL:    delbtp();'),
		+> g('                goto backtrack;'),
		+> g('  case RETRY:   FP[~w].ctrl=NEXT_CALL;',[L1]),
		+> g('                restore();'),
		+> g('}\n'), !.

code_FPr_det([F,N,C]) :+
		+> comm('code for ~w/~w',F,N),
		+> flag(arg,_,arg), flag(arg,_,arg),
		flag(rho,_,N),
		label(F,N,Li), flag(curr_C,_,Li), label(Li,_),
		+> fl(Li),
		map_pred(F/N,Pm), map_fun(F/N,Fm),
		( exported(F/N)
	          -> +> g0('asm(".global PRED~w");',[Pm])
	          ;  true
	        ),
	        +> g0('asm("PRED~w:" : : "p" (&&~w_1));',[Pm,Fm]),
		getlabel1(F,N,Lo),
		btinit(single,Lo,N),
		+> pushenv(single,0,N),
		make_ARGs(N,Args),
		+> g('if (!~w(~w))',[C,Args]),
		+> g('  goto backtrack;'),
		fin(single),
		+> nl, !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

code_C(F,N,cl(La,G),T)	:+
	maplist(trans,La,Lt),
	trans_term(G,Gt),
	vars(Lt,Gt,R),
	label(F,N,Li), flag(curr_C,_,Li), label(Li,_),
	+> fl(Li),
	+> flag(type_cl,_,T),
	( T==single -> flag(rho,_,0) ; flag(rho,_,N+4) ),
	( (T==single; T==first)
	  -> map_pred(F/N,Pm), map_fun(F/N,Fm),
	     ( exported(F/N) -> +> g0('asm(".global PRED~w");',[Pm]); true ),
	     +> g0('asm("PRED~w:" : : "p" (&&~w_1));',[Pm,Fm]),
	     +> flag(arg,_,arg)
	  ;  +> flag(arg,_,fp4)
	),
	getlabel1(F,N,Lo),
	btinit(T,Lo,N),
	+> pushenv(T,R,N),
	mapli(0,code_UA,Lt),
	+> flag(arg,_,arg),
	code_G(Gt),
	fin(T),
	+> nl,
	del(vars_list).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reset_fvar(E)	:+ code_X(E,X),
		   +> reset_var(X).


code_G((G1,G2))		:+ code_G(G1),
			   code_G(G2).
code_G((C->T))		:+ code_G((C->T;fail)).
code_G((G1;G2))		:+ find_fvar(G1,Fv),
			   +> alt_0(L_),
			   code_G(G1,L),
			   code_G_or(G2,Fv,L_,L).

code_G(\+(G))		:+ code_G((G->fail;true)).
code_G(not(G))		:+ code_G(\+(G)).

code_G(G)		:+ inline(G).	% for inlined builtin code
code_G(G)		:+ code_call(G,L),
			   flag(curr_C,C,C), label(C,L),
			   +> fl(L).
			   
code_G((G1,G2),L)	:+ code_G(G1),
			   code_G(G2,L).
code_G((G1->G2),L)	:+ code_G(G1),
			   +> cut,
			   code_G(G2,L).
code_G((G1;G2),K)	:+ find_fvar(G1,Fv),
			   +> alt_0(L_),
			   code_G(G1,L),
			   code_G_or(G2,Fv,L_,L),
			   +> jump(K).
code_G(\+(G),K)		:+ code_G((G->fail;true)),
			   +> jump(K).
code_G(not(G),K)	:+ code_G(\+(G)),
			   +> jump(K).
code_G(G,K)		:+ inline(G),
			   +> jump(K).
code_G(G,L)		:+ code_call(G,L).


code_G_or(fail,F,L_,L)	:+ L_=backtrack,
			   +> alt_2,
			   mapl(reset_fvar,F),
			   flag(curr_C,Li,Li), label(Li,L),
			   +> fl_(L).

code_G_or((G1;G2),F,L_,L) :+ flag(curr_C,Li,Li), label(Li,L_),
			   +> fl(L_),
			   +> alt_1(Lb),
			   mapl(reset_fvar,F),
			   find_fvar(G1,Fv),
			   code_G(G1,L),
			   code_G_or(G2,Fv,Lb,L).

code_G_or(G,F,L_,L)	:+ flag(curr_C,Li,Li), label(Li,L_),
			   +> fl(L_),
			   +> alt_2,
			   mapl(reset_fvar,F),
			   code_G(G,L),
			   flag(curr_C,Li,Li), label(Li,L),
			   +> fl_(L).


code_call(G,L)		:+ fun(G,F,N,A),
			   check_module(F/N),
			   flag(curr_C,C,C), getlabel(C,Lab),
			   +> comm(Lab),
			   flag(meta,Meta,Meta),
			   ( meta_pred(F/N,I),
			     Meta\=true
			     -> flag(current_module,Mod,Mod),
			        add_module(Mod,I,A,Arg)
			     ;  Arg=A
			   ),
			   mapli(0,code_Arg,Arg),
			   +> call_(F,N,L).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
code_Q(Q)	:- trans_term(Q,Qt),
		   vars(Qt,R), L is R+4,
		   flag(rho,_,4), flag(curr_C,_,query), label(query,_),
		   T=[init, comm('code for query'), pushenv(L)|Tq],
		   code_G(Qt,Tq,[ halt_, failed]),
		   trad(T),
		   del(vars_list).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

code_binding(B)	:+ +> g0('\n\nchar *freevar[] =\n{'),
		   mapl(binding,B),
	           +> g0('};\n'),
	           +> g0('int nbr_fv=sizeof(freevar)/sizeof(freevar[0]);\n\n').

binding(N=var(_,I))	:+ J is I-1,
	                  +> g0('  [~w] "~w",',[J,N]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

btinit(first,L,N)	:+ +> setbtp(L),
			   +> saveargs(N).
btinit(middle,L,_)	:+ +> nextalt(L).
btinit(last,_,_)	:+ +> delbtp.
btinit(single,_,_)	:+ true.

fin(last)	:+ +> popenv.
fin(single)	:+ +> popenv.
fin(first)	:+ +> restore.
fin(middle)	:+ +> restore.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
