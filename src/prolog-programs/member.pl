member(X, list(X, T)).
member(X, list(H, T)) :- member(X, T).