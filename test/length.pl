test(Li, Ni, Lo, No) :-
	(
		length(Li, Ni)
	->
		(
			Ni = No
		->
			true
		;
			format('KO: ~w != ~w in ~w\n', [Ni, No, Li])
		),
		(
			Li = Lo
		->
			true
		;
			format('KO: ~w != ~w\n', [Li, Lo])
		),
		format('OK: ~w == ~w in ~w\n', [Ni, No, Li])
	;
		format('KO: length(~w, ~w)\n', [Li, Ni])
	).


test :- test([], _, [], 0).
test :- test([A], _, [A], 1).
test :- test([a,b,c,d], _, _, 4).
test :- test([], 0, [], 0).
test :- test([A], 1, [A], 1).
test :- test([a,b,c,d], 4, _, 4).
test :- test([a,b|_], 4, _, _).
test :- test([1,2|_], 2, _, _).

main :- test, fail; true.
