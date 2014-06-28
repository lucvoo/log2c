/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module(code, [
		code_Arg/4,
		code_Assign/4,
		code_AssignD/4,
		code_M/4,
		code_U/4,
		code_UA/4,
		code_X/2
	]).

:- use_module(aux).
:- use_module(errmsg).
:- use_module(vars).
:- use_module(arith).
:- use_module(mapli).

:- op(1200, xfx, :+).
:- op(900, fy, +>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
code_Assign(V, E) :+
	code_X(E, X),
	+> assign(X, V), !.
code_Assign(V, E) :+
	E=fun(F, Nf, A),
	mapli(0, code_U, A, L),
	+> assign(struct(F, Nf, L), V).

code_AssignD(V, E) :+
	code_X(E, X),
	+> assignD(X, V), !.
code_AssignD(V, E) :+
	E=fun(F, Nf, A),
	mapli(0, code_U, A, L),
	+> assignD(struct(F, Nf, L), V).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
code_X(atom(E), atom(E)).
code_X(intg(E), intg(E)).
code_X(flt(E), flt(E)).
code_X(E, X) :-
	get_lv(O, Nv, E, R),
	flag(rho, L, L),
	K is L+Nv,
	(
		O==f
	->
		X=var(K),
		set_lv(r, Nv, E, R)
	;
		
		O==ft
	->
		X=var_t(Nv),
		set_lv(rt, Nv, E, R)
	;
		
		O==v
	->
		X=void
	;
		
		O==rt
	->
		X=ref_t(Nv)
	;
		
		O==r
	->
		X=ref(K)
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
code_M(V, E) :+
	E=intg(I),
	+> g('~w=~d;', [V, I]).
code_M(V, E) :+
	E=flt(I),
	warning('No math for float'),
	+> g('~w=~d;', [V, I]).
code_M(V, E) :+
	get_lv(O, Nv, E, _),
	flag(rho, L, L),
	(
		O==f
	->
		warning('unbound var in math')
	;
		
		O==ft
	->
		warning('unbound var in math')
	;
		
		O==v
	->
		warning('void var in math')
	;
		
		O==rt
	->
		+> g('GETINTG(TMP_~d,~w);', [Nv, V])
	;
		
		O==r
	->
		I is L+Nv,
		+> g('GETINTG(FP[~d].celp,~w);', [I, V])
	).
code_M(V, E) :+
	E=fun(F, 2, [A, B]),
	map_arith_op(F/2, Op),
	gensym(i_, J),
	+> g('{ int ~w;', [J]),
	+> new_indent(2),
	code_M(V, A),
	code_M(J, B),
	+> g('~w=~w(~w,~w);', [V, Op, V, J]),
	+> new_indent(-2),
	+> g('}').
code_M(V, E) :+
	E=fun(F, 1, [A]),
	map_arith_op(F/1, Op),
	code_M(V, A),
	+> g('~w=~w(~w);', [V, Op, V]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
code_U(I, E) :+
	code_X(E, X),
	+> u(X, I), !.
code_U(I, E) :+
	E=fun(F, N, A),
	mapli(0, code_U, A, L),
	+> u(struct(F, N, L), I).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

code_UA(I, E) :+
	code_X(E, X),
	+> get_(I, X), !.
code_UA(I, E) :+
	E=fun(F, Nf, A),
	mapli(0, code_U, A, L),
	+> get_(I, struct(F, Nf, L)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
code_Arg(I, E) :+
	code_X(E, X),
	+> load_(I, X), !.
code_Arg(I, E) :+
	E=fun(F, Nf, A),
	mapli(0, code_U, A, L),
	+> load_(I, struct(F, Nf, L)).
