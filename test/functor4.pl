test(T, F, N, As) :-
	(
		'$functor'(T,Tf,Tn,Tas)
	->
		(
			F == Tf
		->
			true
		;
			format('KO: functor ~w != ~w in ~w\n', [Tf, F, T])
		),
		(
			N == Tn
		->
			true
		;
			format('KO: arity ~w != ~w in ~w\n', [Tn, N, T])
		),
		(
			As == Tas
		->
			true
		;
			format('KO: args ~w != ~w in ~w\n', [Tas, As, T])
		),
		F == Tf, N == Tn, As == Tas,
		format('OK: \'$functpr\'(~w,~w,~w,~w)\n', [T,F,N,As])
	;
		format('KO: \'$functpr\'(~w,_,_,_)\n', [T])
	).


test :- test(A,_,_,_).
test :- test(a,_,_,_).
test :- test(1,_,_,_).
test :- test(t(a),t,1,[a]).

main :- test, fail; true.
