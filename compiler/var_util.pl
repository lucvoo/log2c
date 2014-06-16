/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module(var_util, [ member_v/2
                    , memberchk_v/2
                    , subtract_v/3
                    , list_to_set_v/2
                    , union_v/3
                    , add_v/3
                    ]).


member_v(E,[F|_])	:- E==F.
member_v(E,[_|Q])	:- member_v(E,Q).

memberchk_v(E,[F|_])	:- E==F, !.
memberchk_v(E,[_|Q])	:- memberchk_v(E,Q).


subtract_v([E|Q],D,O)	:- ( memberchk_v(E,D) -> O=Qo; O=[E|Qo] ),
			   subtract_v(Q,D,Qo).
subtract_v([],_,[]).


list_to_set_v([E|Q],L)	:- ( memberchk_v(E,Q) -> L=Qo; L=[E|Qo] ),
		           list_to_set_v(Q,Qo).
list_to_set_v([],[]).


union_v([],U,U).
union_v([A|Q],I,O)	:- memberchk_v(A,I)
			   -> union_v(Q,I,O)
			   ;  union_v(Q,[A|I],O).

add_v(V,I,O)	:- memberchk_v(V,I)
		   -> I=O
		   ;  O=[V|I].
