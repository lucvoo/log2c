%% Copyright (c) 1997 Luc Van Oostenryck. All rights reserved.
%%

:- module(hash, [ init_hash_atoms/1
		, init_hash_funs/1
		, init_hash_jmps/0
		, init_hash_mods/1
		, init_hash/0
		]).

:- use_module([atoms,map_name,aux,modules,util]).

%% initalize default hash-table size.
init_hash	:- flag(atoms_hash_size,_,2048),
		   flag(funs_hash_size,_,1024),
		   flag(jmps_all_hash_size,_,128),
		   flag(jmps_pub_hash_size,_,32),
		   flag(mods_hash_size,_,32).


init_hash_atoms(As)	:- flag(atoms_hash_size,HS,HS),
			   length(As,N),
			   fill(HS,[],L), 
			   V=..[vec|L],
			   hash_atom_(As,HS,V),
			   hash_atom_vec(V,HS,N).

hash_atom_([],_,_).
hash_atom_([A|Q],HS,V)	:- hpjw(A,H_), H is (H_ mod HS)+1,
			   arg(H,V,E), setarg(H,V,[A|E]),
			   hash_atom_(Q,HS,V).
			   
hash_atom_vec(V,HS,N)	:-
		V=..[vec|L], map(hash_atom_list,L), nl,
		format('atom_t PL__atoms[]=\n{\n'),
		map(hash_atom_tab,L),
		format('};\nint PL__atoms_hash_size = ~d;\n',[HS]),
		format('int PL__atoms_count = ~d;\n\n', [N]).

hash_atom_list([]).
hash_atom_list([A])	:- print_atom_list(A,0).
hash_atom_list([A,B|Q])	:- hash_atom_list([B|Q]),
			   map_atom(B,Bm), concat_atom(['ATOM(',Bm,')'],Nxt),
			   print_atom_list(A,Nxt).

print_atom_list(A,Nxt)	:- map_atom(A,Am), noescape(A,As), hpjw(A,H),
   format('atom__t ATOM_~w={ {(ato_tag<<29)+(uint) ATOM(~w)}, "~w" , ~w ,~w};\n',[Am,Am,As,H,Nxt]).

hash_atom_tab([])	:- format('  0,\n').
hash_atom_tab([E|_])	:- map_atom(E,Em),
			   format('  ATOM(~w),\n',[Em]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_hash_funs(Fs)	:- flag(funs_hash_size,HS,HS),
			   length(Fs, N),
			   fill(HS,[],L), 
			   V=..[vec|L],
			   hash_fun_(Fs,HS,V),
			   hash_fun_vec(V,HS,N).

hash_fun_([],_,_).
hash_fun_([(F/N)|Q],HS,V)	:-
		hpjw(F,H_), H is ((H_ +N) mod HS)+1,
		arg(H,V,E), setarg(H,V,[(F/N)|E]),
		hash_fun_(Q,HS,V).
			   
hash_fun_vec(V,HS,N)	:-
		V=..[vec|L], map(hash_fun_list,L), nl,
		format('fun_t PL__funs[~d]=\n{\n',[HS]),
		map(hash_fun_tab,L),
		format('};\nint PL__funs_hash_size = ~d;\n',[HS]),
		format('int PL__funs_count = ~d;\n\n',[N]).

hash_fun_list([]).
hash_fun_list([F])	:- print_fun_list(F,0).
hash_fun_list([F,G|Q])	:- hash_fun_list([G|Q]),
			   G=Gf/N, map_atom(Gf,Gm),
			   concat_atom(['FUN(',Gm,',',N,')'],Nxt),
			   print_fun_list(F,Nxt).

print_fun_list(F/N,Nxt)	:- map_atom(F,Na),
			   format('fun__t FUN_~w_~w={ ATOM(~w), ~w, ~w};\n',[Na,N,Na,N,Nxt]).
				
hash_fun_tab([])	:- format('  0,\n').
hash_fun_tab([F/N|_])	:- map_atom(F,Fm),
			   format('  FUN(~w,~w),\n',[Fm,N]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_hash_jmps	:- flag(current_module,M,M),
		   map_atom(M,Mm),
		   '$recorded_all'(export_pred,Ppub),
		   findall(P,find_pred(P), Pall_),
		   sort(Pall_,Pall),
		   map(decl_pred,Pall), nl,
		   
		   flag(jmps_pub_hash_size,Hp,Hp),
		   concat('JMP_pub',Mm,Tp),
		   hash_jmp(Hp,Ppub,Tp),

		   flag(jmps_all_hash_size,Ha,Ha),
		   concat('JMP_all',Mm,Ta),
		   hash_jmp(Ha,Pall,Ta),
		   format('module_t module~w = { __FILE__, ATOM(~w), {~w_tab, ~w}, {~w_tab, ~w}};\n',[Mm,Mm,Tp,Hp,Ta,Ha]).

find_pred(P)	:- recorded(preds,P).
find_pred(P)	:- recorded(module_export,module_export(_,P)).

hash_jmp(H,Ps,T)	:- fill(H,[],L), 
			   V=..[vec|L],
			   hash_jmp_(Ps,H,V),
			   hash_jmp_vec(V,T).


hash_jmp_([],_,_).
hash_jmp_([(F/N)|Q],HS,V)
			:- hpjw(F,H_), H is ((H_ +N) mod HS)+1,
			   arg(H,V,E), setarg(H,V,[(F/N)|E]),
			   hash_jmp_(Q,HS,V).
			   
hash_jmp_vec(V,T)	:- V=..[vec|L], map(hash_jmp_list(T),L), nl,
			   format('jmp__t *~w_tab[]=\n{\n',[T]),
			   map(hash_jmp_tab(T),L),
			   format('};\n\n').


hash_jmp_list(_,[]).
hash_jmp_list(T,[F])	:- print_jmp_list(T,F,0).
hash_jmp_list(T,[F,G|Q]):- hash_jmp_list(T,[G|Q]),
			   map_fun(G,Gm),
			   concat_atom(['&',T,Gm],Nxt),
			   print_jmp_list(T,F,Nxt).

print_jmp_list(T,F/N,X)	:- map_fun(F/N,Fm),
			   map_atom(F,Am),
			   import_from_module(F/N,M),
			   map_pred(F/N,M,Pm),
			   format('static jmp__t ~w~w={ ATOM(~w), ~d, &PRED~w, ~w};\n',[T,Fm,Am,N,Pm,X]).
				
hash_jmp_tab(_,[])	:- format('  0,\n').
hash_jmp_tab(T,[E|_])	:- map_fun(E,Em),
			   format('  &~w~w,\n',[T,Em]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_hash_mods(N)	:- need_modules(M),
			   map(include_module,M),
			   select(M,N,M_), Ms=[user|M_],
			   map(decl_mod,Ms), nl,
			   flag(mods_hash_size,HS,HS),
			   fill(HS,[],L), 
			   V=..[vec|L],
			   hash_mods_(Ms,HS,V),
			   hash_mods_vec(V).

decl_mod(M)	:-	   map_atom(M,Mm),
			   format('extern module_t module~w;~n',Mm).

hash_mods_([],_,_).
hash_mods_([A|Q],HS,V)	:- hpjw(A,H_), H is (H_ mod HS)+1,
			   arg(H,V,E), setarg(H,V,[A|E]),
			   hash_mods_(Q,HS,V).
			   
hash_mods_vec(V)	:- V=..[vec|L], map(hash_mods_list,L), nl,
			   format(' modules_t *PL__modules[]=\n{\n'),
			   map(hash_mods_tab,L),
			   format('};\nint PL__modules_hash_size=sizeof(PL__modules)/sizeof(PL__modules[0]);\n\n').

hash_mods_list([]).
hash_mods_list([A])	:- print_mods_list(A,0).
hash_mods_list([A,B|Q])	:- hash_mods_list([B|Q]),
			   map_atom(B,Bm), concat_atom(['&MODULE_',Bm],Nxt),
			   print_mods_list(A,Nxt).

print_mods_list(A,Nxt)	:- map_atom(A,Am),
	format('static modules_t MODULE_~w={ ATOM(~w), &module~w ,~w};\n',[Am,Am,Am,Nxt]).

hash_mods_tab([])	:- format('  0,\n').
hash_mods_tab([E|_])	:- map_atom(E,Em),
			   format('  &MODULE_~w,\n',[Em]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


fill(N,E,L)	:- length(L,N), fill(L,E).
fill([],_).
fill([E|Q],E)	:- fill(Q,E).
