/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module(arith,[ map_arith_op/2
                ]).
:- use_module([map_name,aux]).

map_arith_op(O,N)	:- O = F/A, is_arith_op(F, A), !,
			   map_fun(O,M),
			   concat('PL_OP',M,N).
map_arith_op(O,_)	:- error('Unknow Arithmetic Operator : ~w',[O]).


is_arith_op((*), 2).
is_arith_op((**), 2).
is_arith_op((+), 2).
is_arith_op((-), 1).
is_arith_op((-), 2).
is_arith_op((/), 2).
is_arith_op((//), 2).
is_arith_op((/\), 2).
is_arith_op((<<), 2).
is_arith_op((>>), 2).
is_arith_op((\), 1).
is_arith_op((\/), 2).
is_arith_op((^), 2).
is_arith_op((abs), 1).
is_arith_op((max), 2).
is_arith_op((min), 2).
is_arith_op((mod), 2).
is_arith_op((rem), 2).
is_arith_op((sign), 1).
is_arith_op((xor), 2).
