nat(zero).
nat(X) :- nat(Y), is(X, succ(Y)).

is(X, X).