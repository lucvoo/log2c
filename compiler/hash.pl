/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

:- module(hash, [
		init_hash/0,
		init_hash_atoms/1,
		init_hash_funs/1,
		init_hash_jmps/0,
		init_hash_mods/1
	]).

:- use_module(map_name).
:- use_module(modules).
:- use_module(util).

%% initalize default hash-table size.
init_hash :-
	flag(atoms_hsize, _, 2048),
	flag(funs_hsize, _, 1024),
	flag(jmps_all_hsize, _, 128),
	flag(jmps_pub_hsize, _, 32),
	flag(mods_hsize, _, 32).

/****************************************************************/

init_hash_atoms(As) :-
	flag(atoms_hsize, HS, HS),
	length(As, N),
	fill(HS, [], L),
	V=..[vec|L],
	hash_atom_(As, HS, V),
	hash_atom_vec(V, HS, N).

hash_atom_([], _, _).
hash_atom_([A|Q], HS, V) :-
	hpjw(A, H_),
	H is H_ mod HS+1,
	arg(H, V, E),
	setarg(H, V, [A|E]),
	hash_atom_(Q, HS, V).
			   
hash_atom_vec(V, HS, N) :-
	V=..[vec|L],
	maplist(hash_atom_list, L),
	nl,
	format('struct atom *PL__atoms[]=\n{\n'),
	maplist(hash_atom_tab, L),
	format('};\nint PL__atoms_hash_size = ~d;\n', [HS]),
	format('int PL__atoms_count = ~d;\n\n', [N]).

hash_atom_list([]).
hash_atom_list([A]) :-
	print_atom_list(A, 0).
hash_atom_list([A, B|Q]) :-
	hash_atom_list([B|Q]),
	map_atom(B, Bm),
	concat_atom(['ATOM(', Bm, ')'], Nxt),
	print_atom_list(A, Nxt).

print_atom_list(A, Nxt) :-
	map_atom(A, Am),
	noescape(A, As),
	hpjw(A, H),
	format('struct atom ATOM_~w={ {MK_CELL(ato_tag, ATOM(~w))}, "~w" , ~w ,~w};\n', [Am, Am, As, H, Nxt]).

hash_atom_tab([]) :-
	format('  0,\n').
hash_atom_tab([E|_]) :-
	map_atom(E, Em),
	format('  ATOM(~w),\n', [Em]).

/****************************************************************/

init_hash_funs(Fs) :-
	flag(funs_hsize, HS, HS),
	length(Fs, N),
	fill(HS, [], L),
	V=..[vec|L],
	hash_fun_(Fs, HS, V),
	hash_fun_vec(V, HS, N).

hash_fun_([], _, _).
hash_fun_([F/N|Q], HS, V) :-
	hpjw(F, H_),
	H is (H_+N)mod HS+1,
	arg(H, V, E),
	setarg(H, V, [F/N|E]),
	hash_fun_(Q, HS, V).
			   
hash_fun_vec(V, HS, N) :-
	V=..[vec|L],
	maplist(hash_fun_list, L),
	nl,
	format('struct functor *PL__funs[~d]=\n{\n', [HS]),
	maplist(hash_fun_tab, L),
	format('};\nint PL__funs_hash_size = ~d;\n', [HS]),
	format('int PL__funs_count = ~d;\n\n', [N]).

hash_fun_list([]).
hash_fun_list([F]) :-
	print_fun_list(F, 0).
hash_fun_list([F, G|Q]) :-
	hash_fun_list([G|Q]),
	G=Gf/N,
	map_atom(Gf, Gm),
	concat_atom(['FUN(', Gm, ',', N, ')'], Nxt),
	print_fun_list(F, Nxt).

print_fun_list(F/N, Nxt) :-
	map_atom(F, Na),
	format('struct functor FUN_~w_~w={ ATOM(~w), ~w, ~w};\n', [Na, N, Na, N, Nxt]).
				
hash_fun_tab([]) :-
	format('  0,\n').
hash_fun_tab([F/N|_]) :-
	map_atom(F, Fm),
	format('  FUN(~w,~w),\n', [Fm, N]).

/****************************************************************/

init_hash_jmps :-
	flag(current_module, M, M),
	map_atom(M, Mm),
	'$recorded_all'(export_pred, Ppub),
	findall(P, find_pred(P), Pall_),
	sort(Pall_, Pall),
	maplist(decl_pred, Pall),
	nl,
	flag(jmps_pub_hsize, Hp, Hp),
	hash_jmp(Hp, Ppub, 'JMP_pub'),
	flag(jmps_all_hsize, Ha, Ha),
	hash_jmp(Ha, Pall, 'JMP_all'),
	format('struct module module~w = { __FILE__, ATOM(~w), ', [Mm, Mm]),
	format('{JMP_pub_tab, ~w}, {JMP_all_tab, ~w}};\n', [Hp, Ha]).

find_pred(P) :-
	recorded(preds, P).
find_pred(P) :-
	recorded(module_export, module_export(M, P)),
	M\=system.

hash_jmp(H, Ps, T) :-
	fill(H, [], L),
	V=..[vec|L],
	hash_jmp_(Ps, H, V),
	hash_jmp_vec(V, T).


hash_jmp_([], _, _).
hash_jmp_([F/N|Q], HS, V) :-
	hpjw(F, H_),
	H is (H_+N)mod HS+1,
	arg(H, V, E),
	setarg(H, V, [F/N|E]),
	hash_jmp_(Q, HS, V).
			   
hash_jmp_vec(V, T) :-
	V=..[vec|L],
	maplist(hash_jmp_list(T), L),
	nl,
	format('static struct jmp *~w_tab[]=\n{\n', [T]),
	maplist(hash_jmp_tab(T), L),
	format('};\n\n').


hash_jmp_list(_, []).
hash_jmp_list(T, [F]) :-
	print_jmp_list(T, F, 0).
hash_jmp_list(T, [F, G|Q]) :-
	hash_jmp_list(T, [G|Q]),
	map_fun(G, Gm),
	concat_atom([&, T, Gm], Nxt),
	print_jmp_list(T, F, Nxt).

print_jmp_list(T, F/N, X) :-
	map_fun(F/N, Fm),
	map_atom(F, Am),
	import_from_module(F/N, M),
	map_pred(F/N, M, Pm),
	format('static struct jmp ~w~w={ ATOM(~w), ~d, &PRED~w, ~w};\n', [T, Fm, Am, N, Pm, X]).
				
hash_jmp_tab(_, []) :-
	format('  0,\n').
hash_jmp_tab(T, [E|_]) :-
	map_fun(E, Em),
	format('  &~w~w,\n', [T, Em]).

/****************************************************************/

init_hash_mods(N) :-
	need_modules(M),
	maplist(include_module, M),
	select(N, M, M_),
	Ms=[user|M_],
	maplist(decl_mod, Ms),
	nl,
	flag(mods_hsize, HS, HS),
	fill(HS, [], L),
	V=..[vec|L],
	hash_mods_(Ms, HS, V),
	hash_mods_vec(V).

decl_mod(M) :-
	map_atom(M, Mm),
	format('extern struct module module~w;~n', Mm).

hash_mods_([], _, _).
hash_mods_([A|Q], HS, V) :-
	hpjw(A, H_),
	H is H_ mod HS+1,
	arg(H, V, E),
	setarg(H, V, [A|E]),
	hash_mods_(Q, HS, V).
			   
hash_mods_vec(V) :-
	V=..[vec|L],
	length(L, N),
	maplist(hash_mods_list, L),
	nl,
	format('struct modules *PL__modules[]=\n{\n'),
	maplist(hash_mods_tab, L),
	format('};\nint PL__modules_hash_size = ~d;\n\n', [N]).

hash_mods_list([]).
hash_mods_list([A]) :-
	print_mods_list(A, 0).
hash_mods_list([A, B|Q]) :-
	hash_mods_list([B|Q]),
	map_atom(B, Bm),
	concat_atom(['&MODULE_', Bm], Nxt),
	print_mods_list(A, Nxt).

print_mods_list(A, Nxt) :-
	map_atom(A, Am),
	format('static struct modules MODULE_~w = { ATOM(~w), &module~w ,~w};\n', [Am, Am, Am, Nxt]).

hash_mods_tab([]) :-
	format('  0,\n').
hash_mods_tab([E|_]) :-
	map_atom(E, Em),
	format('  &MODULE_~w,\n', [Em]).

/****************************************************************/

fill(N, E, L) :-
	length(L, N),
	fill(L, E).
fill([], _).
fill([E|Q], E) :-
	fill(Q, E).
/****************************************************************/
noescape(F, Fs) :-
	atom_codes(F, L),
	noescape_(L, Ls),
	atom_codes(Fs, Ls).
noescape_([], []).
noescape_([92|Q], [92, 92|R]) :-
	noescape_(Q, R).
noescape_([34|Q], [92, 34|R]) :-
	noescape_(Q, R).
noescape_([10|Q], [92, 110|R]) :-
	noescape_(Q, R).
noescape_([E|Q], [E|R]) :-
	noescape_(Q, R).
