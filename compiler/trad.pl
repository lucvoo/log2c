/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module(trad, [
		trad/1
	]).

:- use_module([aux, addr, atoms, map_name]).
:- use_module(modules).
:- use_module(errmsg).
:- use_module(codefmt).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
trad([call_(F, N, L), fl(L), restore|Q]) :-
	execute(F, N), !,
	trad(Q).
trad([call_(F, N, L), fl(L), popenv|Q]) :-
	executend(F, N), !,
	trad(Q).
trad([call_(F, N, L), fl_(L), restore|Q]) :-
	execute(F, N), !,
	trad([fl(L), restore|Q]).
trad([call_(F, N, L), fl_(L), popenv|Q]) :-
	executend(F, N), !,
	trad([fl(L), popenv|Q]).
trad([saveargs(S), pushenv(T, R, N)|Q]) :-
	save_push(S, pushenv(T, R, N), Q, Qq),
	trad(Qq).
trad([I|Q]) :-
	call(I), !,
	trad(Q).
trad([]) :- !.

trad_r([], _).
trad_r([u(E, I)|Q], V) :-
	concat_atom([V, +, I], A),
	u_r(E, A),
	trad_r(Q, V).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reset_var(E) :-
	addr(E, A),
	g('~w=new_var();', [A]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
putintg_m(N, N).
putref_m(V, I) :-
	concat_atom(['putref_m(FP[', I, '].celp)'], V).
putref_t_m(V, I) :-
	concat_atom(['putref_m(TMP_', I, ')'], V).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load_(N, struct(F, Ar, L)) :-
	comm(load_(struct)),
	concat_atom(['PL_ARG(', N, ')'], Addr),
	wrt__(struct(F, Ar, L), Addr).
load_(N, E) :-
	addr(E, A),
	(
		(
			E=ref(_)
		;
			E=ref_t(_)
		)
	->
		g('PL_ARG(~w)=deref(~w);', [N, A])
	;
		g('PL_ARG(~w)=~w;', [N, A])
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
u_r(atom(A), V) :-
	map_atom(A, Am),
	g('uatom_r(ATOM(~w),~w);', [Am, V]).
u_r(intg(N), V) :-
	g('getintg(deref(~w),~w);', [V, N]).
u_r(flt(N), V) :-
	g('getflt(deref(~w),~w);', [V, N]).
u_r(void, _).
u_r(var(N), V) :-
	g('FP[~w].celp=(~w);', [N, V]).
u_r(var_t(N), V) :-
	g('TMP_~w=(~w);', [N, V]).
u_r(ref(N), V) :-
	g('if (!pl_unify(FP[~w].celp,~w)) goto backtrack;', [N, V]).
u_r(ref_t(N), V) :-
	g('if (!pl_unify(TMP_~w,~w)) goto backtrack;', [N, V]).
u_r(struct(F, N, L), V) :-
	comm(u_r(struct)),
	unify(struct(F, N, L), deref(V)).

unify(struct(F, N, L), V) :-
	(
		atom(V),
		(
			concat('PL_ARG(', _, V)
		;
			concat('TMP_', _, V)
		)
	->
		Vn=V,
		g('{ if (is_var(~w))', [Vn])
	;
		gensym(v_, Vn),
		g('{ union cell *~w;', [Vn]),
		g('  ~w=~w;', [Vn, V]),
		g('  if (is_var(~w))', [Vn])
	),
	g('  { ~w->celp = HP;', [Vn]),
	g('    trail(~w);', [Vn]),
	new_indent(4),
	trad_off(struct(F, N, L)),
	new_indent(-4),
	g('  }'),
	g('  else'),
	map_atom(F, Fm),
	g('  if (isfun(FUN(~w,~d),~w))', [Fm, N, Vn]),
	g('  {'),
	new_indent(4),
	trad_r(L, Vn),
	new_indent(-4),
	g('  }'),
	g('  else'),
	g('    goto backtrack;'),
	g('}').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
wrt__(struct(F, N, L), V) :-
	g('{ ~w=HP;', [V]),
	new_indent(2),
	trad_off(struct(F, N, L)),
	new_indent(-2),
	g('}'), !.

wrt_(struct(F, N, L), V) :-
	comm(wrt_(struct)),
	concat_atom(['(', V, ')->celp'], Addr),
	wrt__(struct(F, N, L), Addr).

wrt_(atom(A), V) :-
	map_atom(A, Am),
	g('(~w)->celp=new_atom(ATOM(~w));', [V, Am]).
wrt_(intg(N), V) :-
	g('(~w)->val=__intg(~w);', [V, N]).
wrt_(flt(N), V) :-
	g('(~w)->celp=new_flt(~w);', [V, N]).
wrt_(var(I), V) :-
	g('(~w)->val=__var(); FP[~w].celp=(~w);', [V, I, V]).
wrt_(var_t(I), V) :-
	g('(~w)->val=__var(); TMP_~w=(~w);', [V, I, V]).
wrt_(void, V) :-
	g('(~w)->val=__var();', [V]).
wrt_(ref(I), V) :-
	g('(~w)->celp=FP[~w].celp;', [V, I]).
wrt_(ref_t(I), V) :-
	g('(~w)->celp=TMP_~w;', [V, I]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
trad_off(E) :-
	flag(hp, _, 0),
	tradoff([u(E, 0)]),
	write_off,
	flag(hp, HP, 0),
	g('HP+=~d;', [HP]).

write_off :-
	recorded(tradoff, g(F, A), R), !,
	erase(R),
	g(F, A),
	write_off.
write_off.


tradoff([]) :-
	T=struct(_, _, _),
	recorded(offset, w(T, A), R),
	erase(R),
	flag(hp, HP, HP+1),
	recordz(tradoff, g('HP[~d].celp=(HP+~d);', [A, HP])),
	offset(T, HP).
tradoff([]) :-
	T=flt(_),
	recorded(offset, w(T, A), R),
	erase(R),
	flag(hp, HP, HP+3),
	recordz(tradoff, g('HP[~d].celp=(HP+~d);', [A, HP])),
	offset(T, HP).
tradoff([]).


tradoff([u(struct(F, N, L), _)]) :-
	flag(hp, HP, HP+1),
	offset(struct(F, N, L), HP), !.
tradoff([u(flt(F), _)]) :- !,
	flag(hp, HP, HP+3),
	offset(flt(F), HP).
tradoff([u(struct(F, N, L), _)|Q]) :-
	Q\=[],
	flag(hp, HP, HP+1),
	recorda(offset, w(struct(F, N, L), HP)),
	tradoff(Q).
tradoff([u(flt(F), _)|Q]) :-
	Q\=[],
	flag(hp, HP, HP+1),
	recorda(offset, w(flt(F), HP)),
	tradoff(Q).
tradoff([u(E, _)|Q]) :-
	flag(hp, HP, HP+1),
	offset(E, HP),
	tradoff(Q).
tradoff(X) :-
	error('uncatch tradoff(~w)', [X]).


offset(struct(F, A, L), O) :-
	map_atom(F, Fm),
	recordz(tradoff, g('HP[~w].val=__fun(FUN(~w,~d));', [O, Fm, A])),
	tradoff(L), !.
offset(atom(A), O) :-
	map_atom(A, Am),
	recordz(tradoff, g('HP[~w].val=__atom(ATOM(~w));', [O, Am])).
offset(intg(N), O) :-
	recordz(tradoff, g('HP[~w].val=__intg(~w);', [O, N])).
offset(flt(N), O) :-
	recordz(tradoff, g('HP[~w].val=MK_TAG(flt_tag); get_flt(HP+~w) = ~w;', [O, O, N])).
offset(flt(F), _) :-
	error('No offset(flt(~w))', [F]).
offset(var(I), O) :-
	recorda(tradoff, g('HP[~w].val=__var(); FP[~w].celp=HP+(~w);', [O, I, O])).
offset(var_t(I), O) :-
	recorda(tradoff, g('HP[~w].val=__var(); TMP_~w=HP+(~w);', [O, I, O])).
offset(void, O) :-
	recordz(tradoff, g('HP[~w].val=__var();', [O])).
offset(ref(I), O) :-
	recordz(tradoff, g('HP[~w].celp=FP[~w].celp;', [O, I])).
offset(ref_t(I), O) :-
	recordz(tradoff, g('HP[~w].celp=TMP_~w;', [O, I])).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

assign(struct(F, N, L), V) :-
	comm(assign(struct)),
	g('{ ~w=HP;', [V]),
	new_indent(2),
	trad_off(struct(F, N, L)),
	new_indent(-2),
	g('}'), !.
assign(E, V) :-
	addr(E, A),
	g('~w=~w;', [V, A]).

assignD(struct(F, N, L), V) :-
	assign(struct(F, N, L), V).
assignD(E, V) :-
	addr(E, A),
	(
		(
			E=ref(_)
		;
			E=ref_t(_)
		)
	->
		g('~w=deref(~w);', [V, A])
	;
		g('~w=~w;', [V, A])
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_(N, atom(A)) :-
	map_atom(A, Am),
	mem_arg(N, Arg),
	g('getatom(~w,ATOM(~w));', [Arg, Am]).
get_(N, intg(I)) :-
	mem_arg(N, Arg),
	g('getintg(~w,~w);', [Arg, I]).
get_(N, flt(I)) :-
	mem_arg(N, Arg),
	g('getflt(~w,~w);', [Arg, I]).
get_(N, var(I)) :-
	mem_arg(N, Arg),
	g('FP[~w].celp=~w;', [I, Arg]).
get_(N, var_t(I)) :-
	mem_arg(N, Arg),
	g('TMP_~w=~w;', [I, Arg]).
get_(_, void).
get_(N, ref(I)) :-
	mem_arg(N, Arg),
	g('if (!pl_unify(~w,FP[~w].celp)) goto backtrack;', [Arg, I]).
get_(N, ref_t(I)) :-
	mem_arg(N, Arg),
	g('if (!pl_unify(~w,TMP_~w)) goto backtrack;', [Arg, I]).
get_(N, struct(F, A, L)) :-
	mem_arg(N, Arg),
	comm(get_t(struct)),
	unify(struct(F, A, L), Arg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
map_called(F, P) :-
	(
		recorded(preds, F)
	->
		map_fun(F, Fm),
		concat_atom(['goto ', Fm, '_1'], P)
	;
		import_from_module(F, M),
		map_pred(F, M, Pm),
		concat_atom(['ASM_JMP(PRED', Pm, ')'], P)
	).


call_(F, N, L) :-
	comm(call_, F, N),
	map_called(F/N, P),
	call_(P, L).

call_(P, L) :-
	g('SP[1].cod= &&~w;', [L]),
	g('SP[2].stk=FP;'),
	g('FP=SP+2;'),
	g('~w;', [P]).

execute(F, N) :-
	comm(execute, F, N),
	map_called(F/N, P),
	g('SP[1]=FP[-1];'),
	g('SP[2]=FP[0];'),
	g('FP=SP+2;'),
	g('~w;', [P]).

executend(F, N) :-
	comm(executend, F, N),
	map_called(F/N, P),
	g('if (!(FP > BTP))'),
	g('{ SP[1]=FP[-1];'),
	g('  SP[2]=FP[0];'),
	g('  FP=SP+2;'),
	g('}'),
	g('~w;~n', [P]).

backtrack :-
	fl(backtrack),
	g('FP=BTP;'),
	g('HP=(FP+3)->celp;'),
	g('reset(FP[2].tr);'),
	g('goto *((FP+4)->cod);').

failed :-
	fl(failed_query),
	g('FAILED;\n\n').
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pushenv(K) :-
	g('SP=FP+~w;', [K]), !.

pushenv(single, R, _) :-
	I is R,
	pushenv(I).
pushenv(first, R, N) :-
	I is R+4+N,
	pushenv(I).
pushenv(middle, R, N) :-
	I is R+4+N,
	pushenv(I).
pushenv(last, R, N) :-
	I is R+4+N,
	pushenv(I).

setbtp(L) :-
	g('setbtp(&&~w);', [L]).
nextalt(L) :-
	g('nextalt(&&~w);', [L]).
delbtp :-
	g('delbtp();').
popenv :-
	g('popenv();\n').
restore :-
	g('restore();\n').

init :-
	g('init(&&failed_query);').

halt_ :-
	g0('\n\thalt_();\n').

saveargs(N) :-
	(
		comm(saveargs, N),
		between(1, N, I),
		g('FP[~w+4].celp=PL_ARG(~w);', [I, I]),
		fail
	;
		true
	).

cut :-
	g('cut();').
cut_deep :-
	g('cut_deep();').
cut_1 :-
	g('cut_1();').

jump(L) :-
	g('goto ~w;', [L]).
alt_0(L) :-
	g('alt_0(&&~w);', [L]).
alt_1(L) :-
	g('alt_1(&&~w);', [L]).
alt_2 :-
	g('alt_2();').
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
save_push(S, P, I, O) :-
	get_arg(I, L, Q),
	save(S, L, O, T),
	T=[P|Q].

get_arg([get_(N, var(V))|Q], [get_(N, var(V))|T], O) :-
	get_arg(Q, T, O).
get_arg([get_(N, var_t(V))|Q], [get_(N, var_t(V))|T], O) :-
	get_arg(Q, T, O).
get_arg([get_(_, void)|Q], T, O) :-
	get_arg(Q, T, O).
get_arg(Q, [], Q).

save(0, _, T, T).
save(S, L, O, T) :-
	succ(S1, S),
	save(S1, L, O, Q),
	G=g('FP[4+~w].celp=PL_ARG(~w);', [S, S]),
	(
		member(get_(S, V), L)
	->
		Q=[G, get_(S, V)|T]
	;
		Q=[G|T]
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
