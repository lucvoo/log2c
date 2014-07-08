main :- test.

test :- V=true,
	current_prolog_flag(K, V),
	write(K = V), nl,
	fail.
test :- V=_,
	current_prolog_flag(K, V),
	write(K = V), nl,
	fail.
test.
