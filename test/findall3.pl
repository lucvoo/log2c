test_findall3(Templ, Goal) :- 
	findall(Templ, Goal, L),
	format('findall(~w, ~w) => ~w\n', [Templ, Goal, L]).

test :- test_findall3(A, fact(A)), fail.
test :- test_findall3(B-C, fact(B,C)), fail.
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
