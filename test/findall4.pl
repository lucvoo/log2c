test(Templ, Goal, Tail) :- 
	findall(Templ, Goal, L, Tail),
	format('findall(~w, ~w, L, ~w) => L = ~w\n', [Templ, Goal, Tail, L]).

test :- test(A, fact(A), []), fail.
test :- test(A, fact(A), [tail]), fail.
test :- test(A+B, fact(A,B), []), fail.
test :- test(A+B, fact(A,B), [tail]), fail.
test :- true.

main :- test.

%%
fact(a).
fact(b).
fact(c).

%%
fact(a,1).
fact(b,2).
fact(c,3).
