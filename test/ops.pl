main :- test.


test :-
	current_op(Pre, Type, Name),
	writeq(op(Pre, Type, Name)), nl,
	fail.
test.
