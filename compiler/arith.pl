%% Copyright (c) 1997 Luc Van Oostenryck. All rights reserved.
%%

:- module(arith,[ map_arith_op/2
                ]).
:- use_module([map_name,aux]).

map_arith_op(O,N)	:- is_arith_op(O), !, 
			   map_fun(O,M),
			   concat('PL_OP_',M,N).
map_arith_op(O,_)	:- error('Unknow Arithmetic Operator : ~w',[O]).


is_arith_op(O)	:- member(O, [(+)/2, (-)/1, (-)/2, (*)/2, (/)/2, (//)/2, 
                             mod/2, rem/2,
			     (/\)/2, (\/)/2, xor/2, (\)/1,
                             (<<)/2, (>>)/2,
                             abs/1, sign/1,
                             max/2, min/2
                            ]).
