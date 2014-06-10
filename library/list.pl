:- module('$list', [ length/2
		   , member/2
		   , append/3
		   , select/3
		   , flatten/2
		   ]).


%%
length(L, N) :-
	'$length'(L, N), !.
length(L, N) :-
	var(N),
	length(L, N, 0).	% -:+

length([], N, N).
length([_|L], N, M) :-
	succ(M, P),
	length(L, N, P).

%%
member(E,[E|_]).
member(E,[_|Q])	:- member(E,Q).

%%
append([], L, L).
append([H|T], L, [H|R])	:- append(T, L, R).

%%
select(H, [H|T], T).
select(E, [H|T], [H|R]) :-
	select(E, T, R).

%%
flatten(L, F) :-
	flatten(L, [], F), !.

flatten(V, T, F) :-
	var(V), !,
	F = [V|T].
flatten([H|Q], T, F) :-
	!,
	flatten(H, G, F),
	flatten(Q, T, G).
flatten([], T, T) :-
	!.
flatten(E, T, F) :-
	F = [E|T].
