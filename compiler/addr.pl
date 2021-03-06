/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module(addr, [
		addr/2,
		mem_arg/2
	]).
:- use_module(map_name).

%%%%%%%%
addr(atom(A), R) :-
	map_atom(A, Am),
	concat_atom(['new_atom(ATOM(', Am, '))'], R).
addr(intg(N), R) :-
	concat_atom(['new_intg(', N, ')'], R).
addr(flt(N), R) :-
	concat_atom(['new_flt(', N, ')'], R).
addr(var(I), R) :-
	concat_atom(['FP[', I, '].celp=new_var()'], R).
addr(var_t(I), R) :-
	concat_atom(['TMP_', I, '=new_var()'], R).
addr(void, R) :-
	concat_atom(['new_var()'], R).
addr(ref(I), R) :-
	concat_atom(['FP[', I, '].celp'], R).
addr(ref_t(I), R) :-
	concat_atom(['TMP_', I], R).

%%%%%%%%
mem_arg(N, A) :-
	flag(arg, F, F),
	mem_arg_(F, N, A).

mem_arg_(arg, N, A) :-
	concat_atom(['PL_ARG(', N, ')'], A).
mem_arg_(fp4, N, A) :-
	concat_atom(['FP[', N, '+4].celp'], A).
