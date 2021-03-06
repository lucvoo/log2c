/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module(builtin, [
		inline//1
	]).

:- use_module(map_name).
:- use_module(foreign).
:- use_module(code).

:- op(1200, xfx, :+).
:- op(900, fy, +>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

assign_args(_, []) :+
	true.
assign_args(I, [A|Q]) :+
	concat_atom(['args[', I, ']'], Arg),
	code_AssignD(Arg, A),
	succ(I, J),
	assign_args(J, Q).

inline(!) :+
	(
		flag(rho, R, R),
		R==0
	->
		+> cut
	;
		+> cut_deep
	).

inline(X is Y) :+
	+> comm(is, X, Y),
	+> g('{ union cell *c;'),
	+> g('  int i;'),
	+> new_indent(2),
	code_M(i, Y),
	code_AssignD(c, X),
	+> new_indent(-2),
	(
		(
			X=var(f, _)
		;
			X=var(ft, _)
		)
	->
		+> g('  PL_put_integer(c, i);'),
		+> g('  trail(c);')
	;
		+> g('  getintg(c, i);')
	),
	+> g('}').


inline(atom(X)) :+
	type(atom, X, is_atom).
inline(integer(X)) :+
	type(string, X, is_intg).
inline(float(X)) :+
	type(float, X, is_flt).
inline(number(X)) :+
	type(number, X, is_number).
inline(atomic(X)) :+
	type(atomic, X, is_atomic).
inline(var(X)) :+
	type(var, X, is_var).
inline(nonvar(X)) :+
	type(nonvar, X, '!is_var').
inline(callable(X)) :+
	type(callable, X, is_callable).
inline(compound(X)) :+
	type(compound, X, is_fun).
inline(simple(X)) :+
	type(simple, X, '!is_fun').
inline(cons(X)) :+
	type(cons, X, is_cons).
inline(is_list(X)) :+
	type(list, X, is_list).

%%%%

inline(X=Y) :+
	(
		X=var(v, _)
	;
		Y=var(v, _)
	),
	+> comm(=, X, Y), !.
inline(X=Y) :+
	(
		X=var(f, _)
	;
		X=var(ft, _)
	),
	+> comm(=, X, Y),
	unify_var(X, Y), !.
inline(X=Y) :+
	(
		Y=var(f, _)
	;
		Y=var(ft, _)
	),
	+> comm(=, X, Y),
	unify_var(Y, X), !.
inline(X=Y) :+
	+> comm(=, X, Y),
	+> g('{ union cell *a1, *a2;'),
	+> new_indent(2),
	code_Assign(a1, X),
	code_Assign(a2, Y),
	+> new_indent(-2),
	+> g('  if (!pl_unify(a1,a2))'),
	+> g('    goto backtrack;'),
	+> g('}').

inline(X\=Y) :+
	+> comm(\=, X, Y),
	+> g('{ union cell *a1, *a2;'),
	+> new_indent(2),
	code_Assign(a1, X),
	code_Assign(a2, Y),
	+> new_indent(-2),
	+> g('  if (PL_can_unify(a1,a2))'),
	+> g('    goto backtrack;'),
	+> g('}').

inline(X==Y) :+
	Y=atom(A),
	+> comm(==, X, Y),
	+> g('{ union cell *a1;'),
	+> new_indent(2),
	code_AssignD(a1, X),
	+> new_indent(-2),
	map_atom(A, Am),
	+> g('  if (!isatom(ATOM(~w),a1))', [Am]),
	+> g('    goto backtrack;'),
	+> g('}').
inline(X==Y) :+
	Y=intg(I),
	+> comm(==, X, Y),
	+> g('{ union cell *a1;'),
	+> new_indent(2),
	code_AssignD(a1, X),
	+> new_indent(-2),
	+> g('  if (!isintg(~d,a1))', [I]),
	+> g('    goto backtrack;'),
	+> g('}').

inline(X==Y) :+
	std_eq(==, X, Y).
inline(X\==Y) :+
	std_eq(\==, X, Y).
inline(X@<Y) :+
	std_cmp(@<, X, Y).
inline(X@>Y) :+
	std_cmp(@>, X, Y).
inline(X@=<Y) :+
	std_cmp(@=<, X, Y).
inline(X@>=Y) :+
	std_cmp(@>=, X, Y).

inline(X<Y) :+
	arith_cmp(<, X, Y).
inline(X>Y) :+
	arith_cmp(>, X, Y).
inline(X=<Y) :+
	arith_cmp(=<, X, Y).
inline(X>=Y) :+
	arith_cmp(>=, X, Y).
inline(X=:=Y) :+
	arith_cmp(=:=, X, Y).
inline(X=\=Y) :+
	arith_cmp(=\=, X, Y).

inline(X=@=Y) :+
	str_eq(=@=, X, Y).
inline(X\=@=Y) :+
	str_eq(\=@=, X, Y).

inline(succ(X, Y)) :+
	(
		Y=var(f, _)
	;
		Y=var(ft, _)
	),
	+> comm(succ, X, Y),
	+> g('{ union cell *c, *d;'),
	+> new_indent(2),
	code_AssignD(c, X),
	+> g('if (!is_intg(c)) goto backtrack;'),
	code_AssignD(d, Y),
	+> new_indent(-2),
	+> g('  d->val=__intg(c->val+1);'),
	+> g('  trail(d);'),
	+> g('}').
inline(succ(X, Y)) :+
	(
		X=var(f, _)
	;
		X=var(ft, _)
	),
	+> comm(succ, X, Y),
	+> g('{ union cell *c, *d;'),
	+> new_indent(2),
	code_AssignD(c, Y),
	+> g('if (!is_intg(c)) goto backtrack;'),
	code_AssignD(d, X),
	+> new_indent(-2),
	+> g('  d->val=__intg(c->val-1);'),
	+> g('  trail(d);'),
	+> g('}').
inline(succ(X, Y)) :+
	+> comm(succ, X, Y),
	+> g('{ union cell *d0, *d1;'),
	+> new_indent(2),
	code_Assign(d0, X),
	code_Assign(d1, Y),
	+> new_indent(-2),
	+> g('  Deref(d0);'),
	+> g('  if (is_intg(d0))'),
	+> g('  { if (!PL_unify_intg(d1,get_val(d0)+1))'),
	+> g('      goto backtrack;'),
	+> g('  }'),
	+> g('  else'),
	+> g('  { Deref(d1);'),
	+> g('    if (is_intg(d1))'),
	+> g('    { if (!PL_unify_intg(d0,get_val(d1)-1))'),
	+> g('        goto backtrack;'),
	+> g('    }'),
	+> g('    else goto backtrack;'),
	+> g('  }'),
	+> g('}').


inline(fail) :+
	+> comm(fail),
	+> g('goto backtrack;').

inline(true) :+
	+> comm(true).

inline(apply(Clos, Args)) :+
	(
		Clos=fun(:, 2, _)
	->
		Clos_=Clos
	;
		flag(current_module, M, M),
		Clos_=fun(:, 2, [atom(M), Clos])
	),
	+> comm(apply, Clos, Args),
	+> g('{ union cell *clos, args;'),
	+> g('  void *proc;'),
	+> new_indent(2),
	code_Assign(clos, Clos_),
	code_AssignD(args, Args),
	+> new_indent(-2),
	gensym(label_call_, L),
	+> g('  proc=PL_apply(clos,args);'),
	+> g('  if (!proc) goto backtrack;'),
	+> g('  SP[1].cod= &&~w;', [L]),
	+> g('  SP[2].stk=FP;'),
	+> g('  FP=SP+2;'),
	+> g('  goto *proc;'),
	+> g('~w:', [L]),
	+> g('  ;'),
	+> g('}').

inline(Call) :+
	'$functor'(Call, call, N, [Clos|Arg]),
	(
		Clos=fun(:, 2, _)
	->
		Clos_=Clos
	;
		flag(current_module, M, M),
		Clos_=fun(:, 2, [atom(M), Clos])
	),
	+> comm(Call),
	X is N-1,
	+> g('{ union cell *clos;'),
	+> g('  void *proc;'),
	+> g('  static union cell *args[~d];', [X]),
	+> new_indent(2),
	assign_args(0, Arg),
	code_Assign(clos, Clos_),
	+> new_indent(-2),
	gensym(label_call_, L),
	+> g('  proc=PL_call(clos,~d,args);', [X]),
	+> g('  if (!proc) goto backtrack;'),
	+> g('  SP[1].cod= &&~w;', [L]),
	+> g('  SP[2].stk=FP;'),
	+> g('  FP=SP+2;'),
	+> g('  goto *proc;'),
	+> g('~w:', [L]),
	+> g('  ;'),
	+> g('}').


inline(T) :+
	'$functor'(T, F, N, A),
	foreign_pred(F/N, C, det), !,
	decl(N, Aa),
	+> new_indent(2),
	assign(Aa, A),
	+> new_indent(-2),
	function(C, Aa),
	+> g('    goto backtrack;'),
	+> g('}').


unify_var(X, Y) :+
	+> g('{ union cell *a1, *a2;'),
	+> new_indent(2),
	code_Assign(a1, X),
	code_Assign(a2, Y),
	+> new_indent(-2),
	+> g('  a1->celp = a2;'),
	+> g('  trail(a1);'),
	+> g('}').


decl(0, []) :+
	+> g('{').
decl(N, L) :+
	+> f('{ union cell'),
	decl_(N, L).

decl_(1, [A]) :+
	gensym(a_, A),
	+> format(' *~w;\n', [A]).
decl_(I, [A|Q]) :+
	gensym(a_, A),
	+> format(' *~w,', [A]),
	J is I-1,
	decl_(J, Q).


assign([], []) :+
	true.
assign([A|Qa], [B|Qb]) :+
	code_Assign(A, B),
	assign(Qa, Qb).

function(C, []) :+
	+> g('  if (!~w())', C).
function(C, [A]) :+
	+> g('  if (!~w(~w))', [C, A]).
function(C, [A|Q]) :+
	+> f('  if (!~w(~w', [C, A]),
	function_(Q).

function_([]) :+
	+> format('))\n').
function_([A|Q]) :+
	+> format(', ~w', [A]),
	function_(Q).



arith_cmp(Op, X, Y) :+
	+> comm(Op, X, Y),
	map_C_op(Op, C_Op),
	+> g('{ int x,y;'),
	+> new_indent(2),
	code_M(x, X),
	code_M(y, Y),
	+> new_indent(-2),
	+> g('  if (!(~w ~w ~w))', [x, C_Op, y]),
	+> g('    goto backtrack;'),
	+> g('}').

std_cmp(Op, X, Y) :+
	+> comm(Op, X, Y),
	map_C_op(Op, C_Op),
	+> g('{ union cell *x, *y;'),
	+> new_indent(2),
	code_Assign(x, X),
	code_Assign(y, Y),
	+> new_indent(-2),
	+> g('  if (!(pl_std_cmp(x,y) ~w 0))', C_Op),
	+> g('    goto backtrack;'),
	+> g('}').

std_eq(Op, X, Y) :+
	+> comm(Op, X, Y),
	map_C_op(Op, C_Op),
	+> g('{ union cell *x, *y;'),
	+> new_indent(2),
	code_Assign(x, X),
	code_Assign(y, Y),
	+> new_indent(-2),
	+> g('  if (pl_std_eq(x,y) ~w 0)', C_Op),
	+> g('    goto backtrack;'),
	+> g('}').

str_eq(Op, X, Y) :+
	+> comm(Op, X, Y),
	+> g('{ union cell *x, *y;'),
	+> new_indent(2),
	code_Assign(x, X),
	code_Assign(y, Y),
	+> new_indent(-2),
	(
		Op== (=@=)
	->
		+> g('  if (!pl_struct_eq(x,y))')
	;
		+> g('  if (pl_struct_eq(x,y))')
	),
	+> g('    goto backtrack;'),
	+> g('}').


type(N, X, F) :+
	+> comm(N, X),
	+> g('{ union cell *c;'),
	+> new_indent(2),
	code_AssignD(c, X),
	+> new_indent(-2),
	+> g('  if (!~w(c))', F),
	+> g('    goto backtrack;'),
	+> g('}').
