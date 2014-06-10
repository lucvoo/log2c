%% Copyright (c) 1997 Luc Van Oostenryck. All rights reserved.
%%

:- module(my_dcg, [ translate/2 ]).

:- op(1200,xfx,':+').
:- op( 900, fy,'+>').

dgc(arith_cmp,3).
dgc(assign,2).
dgc(assign_args,2).
dgc(btinit,3).
dgc(code_Arg,2).
dgc(code_Assign,2).
dgc(code_AssignD,2).
dgc(code_C,4).
dgc(code_FPr,0).
dgc(code_FPr_det,1).
dgc(code_FPr_ndet,1).
dgc(code_G,1).
dgc(code_G,2).
dgc(code_G_or,4).
dgc(code_M,2).
dgc(code_Pr,1).
dgc(code_U,2).
dgc(code_UA,2).
dgc(code__Pr,1).
dgc(code_call,2).
dgc(code_fin,0).
dgc(decl,2).
dgc(decl_,2).
dgc(fin,1).
dgc(find_fv_,1).
dgc(function,2).
dgc(function_,1).
dgc(inline,1).
dgc(mapl,2).
dgc(mapli,3).
dgc(mapllist,3).
dgc(off_,2).
dgc(reset_fvar,1).
dgc(std_cmp,3).
dgc(std_eq,3).
dgc(type,3).
dgc(unify_intg,2).
dgc(unify_var ,2).
dgc(str_eq,3).
dgc(type,3).
dgc(unify_var,2).


translate((H :+ G), (Th :- Tg))	:-
	H =.. Lh,
	append(Lh,[Li,Lo],Lth),
	Th =.. Lth,
	tr(Li,Lo,G,Tg).

tr(I,O,(G1,G2),(Tg1,Tg2))	:- tr(I,Lt,G1,Tg1), tr(Lt,O,G2,Tg2).
tr(I,O,(G1|G2),T)		:- tr(I,O,(G1;G2),T).
tr(I,O,(G1->G2),(G1->Tg2))	:- tr(I,O,G2,Tg2).
tr(I,O,(C->G1;G2),T)		:- tr(I,Lt,G1,Tg1), tr(Lt,O,G2,Tg2), T=((C->(Tg1,Lt=O));(I=Lt,Tg2)).
tr(I,O,(G1;G2),((Tg1,Lt=O);(I=Lt,Tg2))) :- tr(I,Lt,G1,Tg1), tr(Lt,O,G2,Tg2).
tr(I,O,not(G),T)		:- tr(I,O,\+(G),T).
tr(I,O,\+(G),\+(Tg))		:- tr(I,O,G,Tg).
tr(I,O,+> E,I=[E|O]).
tr(I,O,G,Tg)	:- functor(G,F,N),
		   ( dgc(F,N) -> G=..L, append(L,[I,O],Lt), Tg=..Lt
			      ;  I=O, Tg=G ).


