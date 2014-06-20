main :-
	expr([1+2, 4*4]).


expr([]).
expr([E|Q]) :-
	A is E,
	format('~w is ~w', [A, E]),
	nl,
	expr(Q).
